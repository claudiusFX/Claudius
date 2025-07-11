(* base.ml *)
open Tsdl

module KeyCodeSet = Set.Make (struct
  type t = Key.t

  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

let show_stats = ref false
let recording_state : Animation.recording_state_t option ref = ref None

type input_state = {
  keys : KeyCodeSet.t;
  events : Event.t list;
      (* Accumulated unified input events for the current frame. *)
  mouse : Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t

type tick_func =
  int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list

(* ----- *)

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

let sdl_init width height title make_fullscreen =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title
    Sdl.Window.(if make_fullscreen then fullscreen else windowed)
  >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w
  >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

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

let render_texture r texture s bitmap =
  let width, height = Screen.dimensions s in
  let scale = Screen.scale s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst =
    Sdl.Rect.create
      ~x:((ow - (width * scale)) / 2)
      ~y:((oh - (height * scale)) / 2)
      ~w:(width * scale) ~h:(height * scale)
  in
  Sdl.render_copy ~dst r texture >|= fun () -> Sdl.render_present r

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
            PlatformKey.of_backend_keycode Sdl.Event.(get e keyboard_keycode)
          in
          poll_all_events (KeyCodeSet.add key keys) mouse
            (Event.KeyDown key :: acc)
      | `Key_up ->
          let key =
            PlatformKey.of_backend_keycode Sdl.Event.(get e keyboard_keycode)
          in
          poll_all_events
            (KeyCodeSet.remove key keys)
            mouse (Event.KeyUp key :: acc)
      | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
          let new_mouse, mouse_events = PlatformMouse.handle_event e mouse in
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

let run title boot tick s =
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
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok (w, r) -> (
      match
        Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height
          Sdl.Texture.access_streaming
      with
      | Error (`Msg e) ->
          Sdl.log "Texture error: %s" e;
          exit 1
      | Ok texture ->
          let bitmap =
            Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
              (width * height)
          in
          let initial_buffer =
            match boot with
            | None -> Framebuffer.init (width, height) (fun _ _ -> 0)
            | Some bfunc -> bfunc s
          in
          let initial_input =
            { keys = KeyCodeSet.empty; events = []; mouse = Mouse.create scale }
          in
          let fps_stats = ref (Stats.create ()) in
          let rec loop t prev_buffer input last_t =
            let now = Sdl.get_ticks () in
            let diff =
              Int32.sub (Int32.of_int (1000 / 60)) (Int32.sub now last_t)
            in
            if Int32.compare diff Int32.zero > 0 then Sdl.delay diff;
            let exit, new_keys, new_mouse, unified_events =
              poll_all_events input.keys input.mouse []
            in
            let current_input =
              { keys = new_keys; events = unified_events; mouse = new_mouse }
            in
            if exit then ()
            else (
              fps_stats :=
                Stats.update ~now:(Unix.gettimeofday ()) ~tick:t !fps_stats;

              show_stats :=
                List.fold_left
                  (fun acc ev ->
                    match ev with Event.KeyUp Key.F1 -> not acc | _ -> acc)
                  !show_stats input.events;

              Screenshot.save_screenshot current_input.events s prev_buffer;

              List.iter
                (function
                  | Event.KeyDown Key.F3 -> (
                      Printf.printf
                        "Enter number of frames to record (default 500): %!";
                      try
                        let line = read_line () in
                        let n =
                          if String.trim line = "" then
                            Animation.max_frames_default
                          else int_of_string line
                        in
                        recording_state := Some (Animation.start_recording n)
                      with Failure _ ->
                        Printf.printf
                          "Invalid input. Recording not started.\n%!")
                  | _ -> ())
                input.events;

              let updated_buffer = tick t s prev_buffer current_input in

              let display_buffer =
                if !show_stats then Stats.render !fps_stats t s updated_buffer
                else updated_buffer
              in

              recording_state :=
                Option.bind !recording_state (fun st ->
                    Animation.record_frame st s display_buffer);

              if
                display_buffer != prev_buffer
                || Framebuffer.is_dirty display_buffer
                || Screen.is_dirty s
              then (
                framebuffer_to_bigarray s display_buffer bitmap;
                (match render_texture r texture s bitmap with
                | Error (`Msg e) -> Sdl.log "Render error: %s" e
                | Ok () -> ());
                Framebuffer.clear_dirty updated_buffer;
                Screen.clear_dirty s);
              (match render_texture r texture s bitmap with
              | Error (`Msg e) -> Sdl.log "Render error: %s" e
              | Ok () -> ());
              loop (t + 1) updated_buffer current_input now)
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

let is_key_pressed input key = KeyCodeSet.mem key input.keys

let was_key_just_pressed input key =
  List.exists
    (function Event.KeyDown k when k = key -> true | _ -> false)
    input.events

let was_key_just_released input key =
  List.exists
    (function Event.KeyUp k when k = key -> true | _ -> false)
    input.events
