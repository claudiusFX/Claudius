(* open Graphics *)
open Tsdl

module KeyCodeSet = Set.Make(struct
  type t = Key.t
  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

type input_state = {
  keys: KeyCodeSet.t;
  mouse: Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t


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

(* ----- *)

let run (title : string) (boot : boot_func option) (tick : tick_func) (s : Screen.t) =
  let make_full = Array.to_list Sys.argv |> List.exists (fun a -> (String.compare a "-f") == 0) in

  let s = match make_full with
  | false -> s
  | true ->
      let w, h = Screen.dimensions s and p = Screen.palette s in
      (match Screen.font s with
       | None -> Screen.create w h 1 p
       | Some f -> Screen.create_with_font w h 1 f p)
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

      let e = Sdl.Event.create () in
      let input = { keys = KeyCodeSet.empty; mouse = Mouse.create () } in

      let rec loop (t : int) (prev_buffer : Framebuffer.t) (input : input_state) last_t = (
        let now = Sdl.get_ticks () in
        let diff = Int32.(sub (of_int (1000 / 60)) (sub now last_t)) in
        if Int32.(compare diff zero) > 0 then Sdl.delay diff;

        let updated_buffer = tick t s prev_buffer input in
        let input = { input with mouse = Mouse.clear_event input.mouse } in

        if (updated_buffer != prev_buffer) || (Framebuffer.is_dirty updated_buffer) then (
          framebuffer_to_bigarray s updated_buffer bitmap;
          (match render_texture r texture s bitmap with
           | Error (`Msg e) -> Sdl.log "Boot error: %s" e
           | Ok () -> ());
          Framebuffer.clear_dirty updated_buffer
        );

        let exit, input =
          match Sdl.poll_event (Some e) with
          | true ->
              (match Sdl.Event.(enum (get e typ)) with
               | `Quit -> (true, input)
               | `Key_down ->
                   let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
                   (false, { input with keys = KeyCodeSet.add key input.keys })
               | `Key_up ->
                 let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
                 (false, { input with keys = KeyCodeSet.remove key input.keys })
               | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
                   let mouse = PlatformMouse.handle_event e input.mouse in
                   (match (Mouse.is_button_pressed mouse Mouse.Left) with
                   | false -> (); | true -> Printf.printf "woo\n");
                   (false, { input with mouse })
               | _ -> (false, input))
          | false -> (false, input)
        in
        if exit then () else loop (t + 1) updated_buffer input now
      ) in
      loop 0 initial_buffer input Int32.zero;
      Sdl.destroy_texture texture;
      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
      Sdl.quit ()
