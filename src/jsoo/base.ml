open Js_of_ocaml
module Base_key = Key
open Claudius

type t = { show_stats : bool; status : Stats.t }

let inner_run title boot tick s =
  let width, height = Screen.dimensions s in

  match Backend.v s title false with
  | Error (`Msg e) ->
      (*Backend.log "Init error: %s" e;*)
      (*exit 1*)
      failwith e
  | Ok backend ->
      let initial_buffer =
        match boot with
        | None -> Framebuffer.init (width, height) (fun _ _ -> 0)
        | Some bfunc -> bfunc s
      in
      let input_state =
        ref { keys = Key.KeyCodeSet.empty; events = []; mouse = Mouse.create 1 }
      in

      let initial_internal_state =
        { show_stats = false; status = Stats.create () }
      in

      ignore
        (Dom_html.addEventListener Dom_html.document Dom_html.Event.keydown
           (Dom_html.handler (fun ev ->
                Console.console##log
                  (Js.string (Printf.sprintf "down %d" ev##.keyCode));
                let key = Base_key.of_backend_keycode ev##.keyCode in
                input_state :=
                  {
                    !input_state with
                    keys = Key.KeyCodeSet.add key !input_state.keys;
                    events = Event.KeyDown key :: !input_state.events;
                  };
                Js._true))
           Js._false);

      ignore
        (Dom_html.addEventListener Dom_html.document Dom_html.Event.keyup
           (Dom_html.handler (fun ev ->
                Console.console##log
                  (Js.string (Printf.sprintf "up %d" ev##.keyCode));
                let key = Base_key.of_backend_keycode ev##.keyCode in
                input_state :=
                  {
                    !input_state with
                    keys = Key.KeyCodeSet.remove key !input_state.keys;
                    events = Event.KeyUp key :: !input_state.events;
                  };
                Js._true))
           Js._false);

      let rec loop internal_state prev_buffer (idx : int) (_t : Js.number_t) =
        let internal_state =
          {
            internal_state with
            status =
              Stats.update ~now:(Unix.gettimeofday ()) ~tick:idx
                internal_state.status;
          }
        in

        let internal_state =
          List.fold_left
            (fun acc ev ->
              match ev with
              | Event.KeyUp Key.F1 ->
                  {
                    internal_state with
                    show_stats = not internal_state.show_stats;
                  }
              | _ -> acc)
            internal_state !input_state.events
        in

        let updated_buffer = tick idx s prev_buffer !input_state in

        let stats_buffer =
          Stats.render internal_state.status internal_state.show_stats idx s
            updated_buffer
        in
        let display_buffer =
          match stats_buffer with None -> updated_buffer | Some b -> b
        in
        (* SDL version deals with recording animations here *)
        if
          display_buffer != prev_buffer
          || Framebuffer.is_dirty display_buffer
          || Screen.is_dirty s
        then ignore (Backend.render backend s display_buffer);

        (* Clear the events, which should only be the actions between
      run loop iterations *)
        input_state := { !input_state with events = [] };

        ignore
          (Dom_html.window##requestAnimationFrame
             (Js.wrap_callback (loop internal_state updated_buffer (idx + 1))))
      in
      ignore
        (Dom_html.window##requestAnimationFrame
           (Js.wrap_callback (loop initial_internal_state initial_buffer 0)))

let run title boot tick s =
  Dom_html.onload (fun () -> inner_run title boot tick s)

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
