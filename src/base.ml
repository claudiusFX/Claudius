(* open Graphics *)
open Tsdl

module KeyCodeSet = Set.Make(struct
  type t = Key.t
  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

type event =
  | KeyDown of Key.t
  | KeyUp of Key.t
  | MouseButtonDown of int
  | MouseButtonUp of int
  | MouseMotion of { x : int; y : int; xrel : int; yrel : int }
  | MouseWheel of { x : int; y : int }

type input_state = {
  keys: KeyCodeSet.t;
  events: event list;
  mouse: Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list

(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init (width : int) (height : int) (title : string) (make_fullscreen : bool) =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title Sdl.Window.(if make_fullscreen then fullscreen else windowed) >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

let framebuffer_to_bigarray (s : Screen.t) (buffer : Framebuffer.t) (bitmap : bitmap_t) =
  let palette = Screen.palette s in
  Array.iteri (fun y row ->
    Array.iteri (fun x pixel ->
      bitmap.{x + (y * (Array.length row))} <- Palette.index_to_rgb palette pixel
    ) row
  ) (Framebuffer.to_array buffer)

let render_texture (r : Sdl.renderer) (texture : Sdl.texture) (s : Screen.t) (bitmap : bitmap_t) =
  let width, height = Screen.dimensions s in
  let scale = Screen.scale s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst = Sdl.Rect.create ~x:((ow - (width * scale)) / 2) ~y:((oh - (height * scale)) / 2) ~w:(width * scale) ~h:(height * scale) in
  Sdl.render_copy ~dst:dst r texture >|= fun () ->
  Sdl.render_present r

(** 3. A helper function [poll_all_events], 
    collecting all SDL events each frame into our unified [event] list. *)
let rec poll_all_events (keys : KeyCodeSet.t) (mouse : Mouse.t) (acc : event list)
  : bool * KeyCodeSet.t * Mouse.t * event list =
  let e = Sdl.Event.create () in
  match Sdl.poll_event (Some e) with
  | true ->
      begin match Sdl.Event.(enum (get e typ)) with
      | `Quit ->
          (true, keys, mouse, List.rev acc)
      | `Key_down ->
          let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
          poll_all_events (KeyCodeSet.add key keys) mouse (KeyDown key :: acc)
      | `Key_up ->
          let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
          poll_all_events (KeyCodeSet.remove key keys) mouse (KeyUp key :: acc)
      | `Mouse_button_down ->
          let button = Sdl.Event.(get e mouse_button_button) in
          let new_mouse = PlatformMouse.handle_event e mouse in
          poll_all_events keys new_mouse (MouseButtonDown button :: acc)
      | `Mouse_button_up ->
          let button = Sdl.Event.(get e mouse_button_button) in
          let new_mouse = PlatformMouse.handle_event e mouse in
          poll_all_events keys new_mouse (MouseButtonUp button :: acc)
      | `Mouse_motion ->
          let x = Sdl.Event.(get e mouse_motion_x) in
          let y = Sdl.Event.(get e mouse_motion_y) in
          let xrel = Sdl.Event.(get e mouse_motion_xrel) in
          let yrel = Sdl.Event.(get e mouse_motion_yrel) in
          let new_mouse = PlatformMouse.handle_event e mouse in
          poll_all_events keys new_mouse (MouseMotion { x; y; xrel; yrel } :: acc)
      | `Mouse_wheel ->
          let x = Sdl.Event.(get e mouse_wheel_x) in
          let y = Sdl.Event.(get e mouse_wheel_y) in
          let new_mouse = PlatformMouse.handle_event e mouse in
          poll_all_events keys new_mouse (MouseWheel { x; y } :: acc)
      | _ ->
          poll_all_events keys mouse acc
      end
  | false ->
      (false, keys, mouse, List.rev acc)

let run (title : string) (boot : boot_func option)
        (tick : tick_func) (s : Screen.t) =
  let make_full = 
    Array.to_list Sys.argv |> List.exists (fun a -> String.compare a "-f" == 0)
  in

  (* Optionally recreate the screen in fullscreen mode with scale=1, if requested. *)
  let s =
    match make_full with
    | false -> s
    | true ->
        let w, h = Screen.dimensions s in
        let p = Screen.palette s in
        match Screen.font s with
        | None -> Screen.create w h 1 p
        | Some f -> Screen.create_with_font w h 1 f p
  in

  let width, height = Screen.dimensions s and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title make_full with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
    match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height Sdl.Texture.access_streaming with
    | Error (`Msg e) -> Sdl.log "texture error: %s" e; exit 1
    | Ok texture ->
      (* This is a conversion layer, but allocaing bigarrays frequently is frowned upon
        so we allocate it once here and re-use it. *)
      let bitmap = (Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)) in

      let initial_buffer = match boot with
      | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
      | Some bfunc -> bfunc s
      in

      let initial_input = {
        keys = KeyCodeSet.empty;
        events = [];
        mouse = Mouse.create scale;
      } in

      let rec loop (t : int)
                   (prev_buffer : Framebuffer.t)
                   (input : input_state)
                   (last_t : int32) : unit =
        let now = Sdl.get_ticks () in
        let diff = Int32.(sub (of_int (1000 / 60)) (sub now last_t)) in
        if Int32.(compare diff zero) > 0 then Sdl.delay diff;

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
          let updated_buffer =
            tick t s prev_buffer current_input
          in

          (* Redraw only if the buffer or screen is dirty (or changed). *)
          if (updated_buffer != prev_buffer)
             || (Framebuffer.is_dirty updated_buffer)
             || (Screen.is_dirty s)
          then (
            framebuffer_to_bigarray s updated_buffer bitmap;
            match render_texture r texture s bitmap with
            | Error (`Msg e) ->
                Sdl.log "Boot error: %s" e
            | Ok () -> ();
            Framebuffer.clear_dirty updated_buffer;
            Screen.clear_dirty s
          );

          match render_texture r texture s bitmap with
          | Error (`Msg e) ->
              Sdl.log "Boot error: %s" e
          | Ok () ->
              loop (t + 1) updated_buffer current_input now
        end
      in

      loop 0 initial_buffer initial_input Int32.zero;

      Sdl.destroy_texture texture;
      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
      Sdl.quit ()

(** 6. “Functional” style run that returns [Primitives.t list] instead of
       a full [Framebuffer.t]. We wrap it to produce a [tick_func]. *)
let run_functional (title : string)
                   (tick_f : functional_tick_func)
                   (s : Screen.t) =
  let wrap_tick (t : int)
                (screen : Screen.t)
                (prev_framebuffer : Framebuffer.t)
                (input : input_state)
              : Framebuffer.t =
    let primitives = tick_f t screen input in
    if primitives = [] then
      prev_framebuffer
    else
      let width, height = Screen.dimensions screen in
      let new_framebuffer = Framebuffer.init (width, height) (fun _x _y -> 0) in
      Framebuffer.render new_framebuffer primitives;
      new_framebuffer
  in
  run title None wrap_tick s