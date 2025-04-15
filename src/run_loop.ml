open Tsdl
open Bigarray
open Base

module MakeRunLoop (SDL : Sdl_api.SDL_API) = struct

  let run (title : string) (boot : Base.boot_func option) (tick : Base.tick_func) (s : Screen.t) =
    let make_full = Array.to_list Sys.argv |> List.exists (fun a -> (String.compare a "-f") = 0) in
    let s =
      match make_full with
      | false -> s
      | true ->
          let w, h = Screen.dimensions s and p = Screen.palette s in
          (match Screen.font s with
           | None -> Screen.create w h 1 p
           | Some f -> Screen.create_with_font w h 1 f p)
    in
    let width, height = Screen.dimensions s and scale = Screen.scale s in
    match Base.sdl_init (width * scale) (height * scale) title make_full with
    | Error (`Msg e) ->
        Sdl.log "Init error: %s" e;
        exit 1
    | Ok (w, r) ->
        match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height Sdl.Texture.access_streaming with
        | Error (`Msg e) ->
            Sdl.log "texture error: %s" e;
            exit 1
        | Ok texture ->
            let bitmap = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height) in
            let initial_buffer =
              match boot with
              | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
              | Some bfunc -> bfunc s
            in
            let e = Sdl.Event.create () in
            let input = { keys = KeyCodeSet.empty; mouse = Mouse.create scale } in
            let rec loop (t : int) (prev_buffer : Framebuffer.t) (input : input_state) last_t =
              let now = Sdl.get_ticks () in
              let diff = Int32.(sub (of_int (1000 / 60)) (sub now last_t)) in
              if Int32.(compare diff zero) > 0 then Sdl.delay diff;
              let keys = KeyCodeSet.elements input.keys in
              Screenshot.save_screenshot keys s prev_buffer;
              let updated_buffer = tick t s prev_buffer input in
              let input = { input with mouse = Mouse.clear_events input.mouse } in
              if (updated_buffer != prev_buffer)
                 || (Framebuffer.is_dirty updated_buffer)
                 || (Screen.is_dirty s)
              then (
                framebuffer_to_bigarray s updated_buffer bitmap;
                (match render_texture r texture s bitmap with
                 | Error (`Msg e) -> Sdl.log "Boot error: %s" e
                 | Ok () -> ());
                Framebuffer.clear_dirty updated_buffer;
                Screen.clear_dirty s
              );
              match render_texture r texture s bitmap with
              | Error (`Msg e) -> Sdl.log "Boot error: %s" e
              | Ok () ->
                  let exit, input =
                    match Sdl.poll_event (Some e) with
                    | true ->
                        (match Sdl.Event.(enum (get e typ)) with
                         | `Quit -> (true, input)
                         | `Key_down ->
                             let key =
                               PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode))
                             in
                             (false, { input with keys = KeyCodeSet.add key input.keys })
                         | `Key_up ->
                             let key =
                               PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode))
                             in
                             (false, { input with keys = KeyCodeSet.remove key input.keys })
                         | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
                             let mouse = PlatformMouse.handle_event e input.mouse in
                             (false, { input with mouse })
                         | _ -> (false, input))
                    | false -> (false, input)
                  in
                  if exit then ()
                  else loop (t + 1) updated_buffer input now
            in
            let _ = loop 0 initial_buffer input Int32.zero in
            Sdl.destroy_texture texture;
            Sdl.destroy_renderer r;
            Sdl.destroy_window w;
            Sdl.quit ()
  ;;

  let run_functional (title : string) (tick_f : functional_tick_func) (s : Screen.t) : Framebuffer.t =
    let wrap_tick (t : int) (screen : Screen.t) (prev_fb : Framebuffer.t) (input : input_state) : Framebuffer.t =
      let primitives = tick_f t screen input in
      if primitives = [] then prev_fb
      else
        let width, height = Screen.dimensions screen in
        let new_fb = Framebuffer.init (width, height) (fun _x _y -> 0) in
        Framebuffer.render new_fb primitives;
        new_fb
    in
    run title None wrap_tick s
  ;;
  
end
