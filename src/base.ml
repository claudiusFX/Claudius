(* base.ml *)
open Tsdl

module KeyCodeSet = Set.Make(struct
  type t = Key.t
  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

type input_state = {
  keys: KeyCodeSet.t;
  events: Event.t list;  (* Accumulated unified input events for the current frame. *)
  mouse: Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list

(* SDL operation  signature*)
module type SDL_ops = sig
  type window
  type renderer
  type texture
  type event
  
  type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

  (**This handle the initialization of SDL
    parameters width, height, window title and fullscreen flag and onsuccess, 
    this returns a window and a renderer and on failure we have an error message
  *)
  val init : int -> int -> string -> bool -> (window * renderer, string) Stdlib.result

(* Create a texture, given a renderer, a pixel format and dimension of width and
   height if successful, a texture is returned else an error*)
  val create_texture : renderer -> int -> width:int -> height:int -> Sdl.Texture.access -> (texture, string) Stdlib.result

   (* Update texture with pixel data from [bitmap] the integer parameter, represents pixel width *)
  val update_texture : renderer -> texture -> bitmap_t -> int -> (unit, string) Stdlib.result
  val renderer_clear : renderer -> (unit, string) Stdlib.result  
  val render_copy    : renderer -> texture -> dst:Sdl.rect option -> (unit, string) Stdlib.result

  val render_present : renderer -> unit
  val poll_event_opt : unit -> event option
  val get_ticks      : unit -> int32
  val delay          : int32 -> unit
  val mk_event       : unit -> event
end

module type Screenshot_ops = sig
  val save : Event.t list -> Screen.t -> Framebuffer.t -> unit
end

(** SDL binding, wiring through  existing Tsdl calls *)
module SDL_impl : SDL_ops = struct
  type window   = Sdl.window
  type renderer = Sdl.renderer
  type texture  = Sdl.texture
  type event    = Sdl.event

  let init w h title fs =
    Sdl.init Sdl.Init.(video + events) >>= fun () ->
    Sdl.create_window ~w ~h title Sdl.Window.(if fs then fullscreen else windowed) >>= fun win ->
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) win >|= fun r -> (win, r)

  let create_texture = Sdl.create_texture

  let render_clear   r          = Sdl.render_clear r
  let update_texture r tx bm w  = Sdl.update_texture r tx bm w
  let render_copy    r tx ~dst  = Sdl.render_copy ~dst r tx

  let poll_event_opt () =
    let e = Sdl.Event.create () in
    if Sdl.poll_event (Some e) then Some e else None

  let get_ticks () = Sdl.get_ticks ()
  let delay      d = Sdl.delay d
  let mk_event () = Sdl.Event.create ()
end

(* Default screenshot implementation *)
module Screenshot_impl : Screenshot_ops = struct
  let save = Screenshot.save_screenshot
end

(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init width height title make_fullscreen =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title Sdl.Window.(if make_fullscreen then fullscreen else windowed) >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

let framebuffer_to_bigarray s buffer bitmap =
  let palette = Screen.palette s in
  Array.iteri (fun y row ->
    Array.iteri (fun x pixel ->
      bitmap.{x + (y * (Array.length row))} <- Palette.index_to_rgb palette pixel
    ) row
  ) (Framebuffer.to_array buffer)

let render_texture r texture s bitmap =
  let width, height = Screen.dimensions s in
  let scale = Screen.scale s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst = Sdl.Rect.create ~x:((ow - (width * scale)) / 2) ~y:((oh - (height * scale)) / 2) ~w:(width * scale) ~h:(height * scale) in
  Sdl.render_copy ~dst:dst r texture >|= fun () ->
  Sdl.render_present r

(* Poll SDL events and build the unified event queue.
   Mouse events are handled by PlatformMouse.handle_event, which returns
   an updated mouse state along with a list of unified events. *)
let rec poll_all_events keys mouse acc =
  let e = Sdl.Event.create () in
  match Sdl.poll_event (Some e) with
  | true ->
      (match Sdl.Event.(enum (get e typ)) with
       | `Quit ->
           (true, keys, mouse, List.rev acc)
       | `Key_down ->
           let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
           poll_all_events (KeyCodeSet.add key keys) mouse (Event.KeyDown key :: acc)
       | `Key_up ->
           let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
           poll_all_events (KeyCodeSet.remove key keys) mouse (Event.KeyUp key :: acc)
       | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
           let (new_mouse, mouse_events) = PlatformMouse.handle_event e mouse in
           poll_all_events keys new_mouse (List.rev_append mouse_events acc)
       | `Drop_file ->
          let filepath = Sdl.Event.drop_file_file e in
          Sdl.Event.drop_file_free e;
          let updated_events = match filepath with
          | None -> acc
          | Some filepath -> Event.DropFile filepath :: acc
          in
          poll_all_events keys mouse updated_events
       | _ ->
           poll_all_events keys mouse acc)
  | false ->
      (false, keys, mouse, List.rev acc)

let run
  ?(sdl_ops : (module SDL_ops) = (module SDL_impl)) 
  ?(screenshot_ops : (module Screenshot_ops) = (module Screenshot_impl))
  title boot tick s =

  let module SDL = (val sdl_ops : SDL_ops) in
  let module SS  = (val screenshot_ops : Screenshot_ops) in

  let make_full =
    Array.to_list Sys.argv |> List.exists (fun a -> String.compare a "-f" = 0)
  in
  let s =
    match make_full with
    | false -> s
    | true ->
        let w, h = Screen.dimensions s in
        let p = Screen.palette s in
        let font = Screen.font s in
        Screen.create ~font w h 1 p
  in

  let width, height = Screen.dimensions s and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title make_full with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
      (match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height
              Sdl.Texture.access_streaming with
       | Error (`Msg e) ->
           Sdl.log "Texture error: %s" e;
           exit 1
       | Ok texture ->
           let bitmap =
             Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
           in
           let initial_buffer =
             match boot with
             | None -> Framebuffer.init (width, height) (fun _ _ -> 0)
             | Some bfunc -> bfunc s
           in
           let initial_input = {
             keys = KeyCodeSet.empty;
             events = [];
             mouse = Mouse.create scale;
           } in
           let rec loop t prev_buffer input last_t =
             let now = Sdl.get_ticks () in
             let diff = Int32.sub (Int32.of_int (1000 / 60)) (Int32.sub now last_t) in
             if Int32.compare diff Int32.zero > 0 then Sdl.delay diff;
             let exit, new_keys, new_mouse, unified_events =
               poll_all_events input.keys input.mouse []
             in
             let current_input = {
               keys = new_keys;
               events = unified_events;
               mouse = new_mouse;
             } in
             if exit then ()
             else begin
               Screenshot.save_screenshot current_input.events s prev_buffer;

               let updated_buffer = tick t s prev_buffer current_input in
               if (updated_buffer != prev_buffer)
                  || (Framebuffer.is_dirty updated_buffer)
                  || (Screen.is_dirty s)
               then (
                 framebuffer_to_bigarray s updated_buffer bitmap;
                 (match render_texture r texture s bitmap with
                  | Error (`Msg e) -> Sdl.log "Render error: %s" e
                  | Ok () -> ());
                 Framebuffer.clear_dirty updated_buffer;
                 Screen.clear_dirty s
               );
               (match render_texture r texture s bitmap with
                | Error (`Msg e) -> Sdl.log "Render error: %s" e
                | Ok () -> ());
               loop (t + 1) updated_buffer current_input now
             end
           in
           loop 0 initial_buffer initial_input Int32.zero;
           Sdl.destroy_texture texture;
           Sdl.destroy_renderer r;
           Sdl.destroy_window w;
           Sdl.quit ())

let run_functional title tick_f s =
  let wrap_tick t screen prev_framebuffer input =
    let primitives = tick_f t screen input in
    if primitives = [] then prev_framebuffer
    else
      let width, height = Screen.dimensions screen in
      let new_framebuffer = Framebuffer.init (width, height) (fun _ _ -> 0) in
      Framebuffer.render new_framebuffer primitives;
      new_framebuffer
  in
  run title None wrap_tick s

(* --- Utility functions for input handling --- *)

let is_key_pressed input key =
  KeyCodeSet.mem key input.keys

let was_key_just_pressed input key =
  List.exists (function
      | Event.KeyDown k when k = key -> true
      | _ -> false
    ) input.events

let was_key_just_released input key =
  List.exists (function
      | Event.KeyUp k when k = key -> true
      | _ -> false
    ) input.events