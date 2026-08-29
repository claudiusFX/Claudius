(* base.ml *)

module Platform = Sdl_backend

type t = {
  show_stats : bool;
  recording_state : Animation.recording_state_t option;
  status : Stats.t;
}

type input_state = {
  keys : Key.KeyCodeSet.t;
  events : Event.t list;
      (* Accumulated unified input events for the current frame. *)
  mouse : Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t

type tick_func =
  int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list

(* ----- *)

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

  match Platform.v s title make_full with
  | Error (`Msg e) ->
      Platform.log "Init error: %s" e;
      exit 1
  | Ok backend ->
      let bitmap =
        Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
      in
      let initial_buffer =
        match boot with
        | None -> Framebuffer.init (width, height) (fun _ _ -> 0)
        | Some bfunc -> bfunc s
      in
      let initial_input =
        { keys = Key.KeyCodeSet.empty; events = []; mouse = Mouse.create scale }
      in

      let initial_internal_state =
        { show_stats = false; recording_state = None; status = Stats.create () }
      in

      let rec loop internal_state t prev_buffer input last_t =
        let now = Platform.get_ticks () in
        let diff =
          Int32.sub (Int32.of_int (1000 / 60)) (Int32.sub now last_t)
        in
        if Int32.compare diff Int32.zero > 0 then Platform.delay diff;
        let exit, new_keys, new_mouse, unified_events =
          Platform.poll_all_events input.keys input.mouse []
        in
        let current_input =
          { keys = new_keys; events = unified_events; mouse = new_mouse }
        in
        if exit then ()
        else
          let internal_state =
            {
              internal_state with
              status =
                Stats.update ~now:(Unix.gettimeofday ()) ~tick:t
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
                | Event.KeyUp Key.F2 ->
                    let log_message =
                      match Screenshot.save_screenshot s prev_buffer with
                      | Result.Ok path ->
                          Printf.sprintf "Screenshot saved as %s" path
                      | Result.Error msg -> msg
                    in
                    {
                      internal_state with
                      status = Stats.log internal_state.status log_message;
                    }
                | Event.KeyUp Key.F3 -> (
                    Printf.printf
                      "Enter number of frames to record (default 500): %!";
                    try
                      let line = read_line () in
                      let n =
                        if String.trim line = "" then
                          Animation.max_frames_default
                        else int_of_string line
                      in
                      match Animation.start_recording n with
                      | Result.Ok recording_state ->
                          {
                            internal_state with
                            recording_state = Some recording_state;
                          }
                      | Result.Error msg ->
                          {
                            internal_state with
                            status = Stats.log internal_state.status msg;
                          }
                    with Failure _ ->
                      {
                        internal_state with
                        status =
                          Stats.log internal_state.status
                            "Invalid input. Recording not started.";
                      })
                | _ -> acc)
              internal_state input.events
          in

          let updated_buffer = tick t s prev_buffer current_input in

          let stats_buffer =
            Stats.render internal_state.status internal_state.show_stats t s
              updated_buffer
          in
          let display_buffer =
            match stats_buffer with None -> updated_buffer | Some b -> b
          in

          let internal_state =
            {
              internal_state with
              recording_state =
                Option.bind internal_state.recording_state (fun st ->
                    Animation.record_frame st s display_buffer);
            }
          in

          if
            display_buffer != prev_buffer
            || Framebuffer.is_dirty display_buffer
            || Screen.is_dirty s
          then (
            framebuffer_to_bigarray s display_buffer bitmap;
            (match Platform.render backend s bitmap with
            | Error (`Msg e) -> Platform.log "Render error: %s" e
            | Ok () -> ());
            Framebuffer.clear_dirty updated_buffer;
            Screen.clear_dirty s);
          (match Platform.render backend s bitmap with
          | Error (`Msg e) -> Platform.log "Render error: %s" e
          | Ok () -> ());
          loop internal_state (t + 1) updated_buffer current_input now
      in
      loop initial_internal_state 0 initial_buffer initial_input Int32.zero;
      Platform.shutdown backend

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

let is_key_pressed input key = Key.KeyCodeSet.mem key input.keys

let was_key_just_pressed input key =
  List.exists
    (function Event.KeyDown k when k = key -> true | _ -> false)
    input.events

let was_key_just_released input key =
  List.exists
    (function Event.KeyUp k when k = key -> true | _ -> false)
    input.events
