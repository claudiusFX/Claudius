open Tsdl
open Claudius

type t = {
  r : Sdl.renderer;
  w : Sdl.window;
  texture : Sdl.texture;
  bitmap : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

let v s title make_fullscreen =
  let width, height = Screen.dimensions s and scale = Screen.scale s in
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:(width * scale) ~h:(height * scale) title
    Sdl.Window.(if make_fullscreen then fullscreen else windowed)
  >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w
  >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >>= fun _ ->
  Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height
    Sdl.Texture.access_streaming
  >|= fun texture ->
  let bitmap =
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
  in

  { w; r; texture; bitmap }

let shutdown t =
  Sdl.destroy_texture t.texture;
  Sdl.destroy_renderer t.r;
  Sdl.destroy_window t.w;
  Sdl.quit ()

let framebuffer_to_bigarray s buffer bitmap =
  let palette = Screen.palette s in
  Array.iteri
    (fun y row ->
      Array.iteri
        (fun x pixel ->
          bitmap.{x + (y * Array.length row)} <-
            Palette.index_to_rgb palette pixel)
        row)
    (Framebuffer.to_array buffer)

let render t screen framebuffer =
  framebuffer_to_bigarray screen framebuffer t.bitmap;

  let width, height = Screen.dimensions screen in
  let scale = Screen.scale screen in
  Sdl.render_clear t.r >>= fun () ->
  Sdl.update_texture t.texture None t.bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size t.r) in
  let dst =
    Sdl.Rect.create
      ~x:((ow - (width * scale)) / 2)
      ~y:((oh - (height * scale)) / 2)
      ~w:(width * scale) ~h:(height * scale)
  in
  Sdl.render_copy ~dst t.r t.texture >|= fun () -> Sdl.render_present t.r

(* Poll SDL events and build the unified event queue.
     Mouse events are handled by PlatformMouse.handle_event, which returns
     an updated mouse state along with a list of unified events. *)
let rec poll_all_events keys mouse acc =
  let e = Sdl.Event.create () in
  match Sdl.poll_event (Some e) with
  | true -> (
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> (true, keys, mouse, List.rev acc)
      | `Key_down ->
          let key =
            Keysdl.of_backend_keycode Sdl.Event.(get e keyboard_keycode)
          in
          poll_all_events
            (Key.KeyCodeSet.add key keys)
            mouse (Event.KeyDown key :: acc)
      | `Key_up ->
          let key =
            Keysdl.of_backend_keycode Sdl.Event.(get e keyboard_keycode)
          in
          poll_all_events
            (Key.KeyCodeSet.remove key keys)
            mouse (Event.KeyUp key :: acc)
      | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
          let new_mouse, mouse_events = Mousesdl.handle_event e mouse in
          poll_all_events keys new_mouse (List.rev_append mouse_events acc)
      | `Drop_file ->
          let filepath = Sdl.Event.drop_file_file e in
          Sdl.Event.drop_file_free e;
          let updated_events =
            match filepath with
            | None -> acc
            | Some filepath -> Event.DropFile filepath :: acc
          in
          poll_all_events keys mouse updated_events
      | _ -> poll_all_events keys mouse acc)
  | false -> (false, keys, mouse, List.rev acc)

let log s = Sdl.log s
let get_ticks () = Sdl.get_ticks ()
let delay d = Sdl.delay d
