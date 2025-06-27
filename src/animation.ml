open Giflib
open Unix
open Utils_gif

type recording_state_t = {
  frames : Image.t list;
  frames_to_record : int;
  current_frame : int;
}

let max_frames_default = 500

let now_string () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "animation_%02d%02d%02d_%02d%02d%02d" (tm.tm_year mod 100)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let start_recording ?(max_frames = max_frames_default)
    (recording_state : recording_state_t option ref) n =
  match !recording_state with
  | Some _ -> failwith "Already recording animation"
  | None ->
      if max_frames <= 0 then failwith "Number of frames must be positive";
      if n <= 0 then failwith "Number of frames must be positive";
      if n > max_frames then
        failwith (Printf.sprintf "Maximum %d frames allowed" max_frames_default);
      recording_state :=
        Some { frames = []; frames_to_record = n; current_frame = 0 };
      Printf.printf "Started recording %d frames\n%!" n

let stop_recording (recording_state : recording_state_t option ref) =
  match !recording_state with
  | None -> failwith "Not recording animation"
  | Some rs ->
      let frames = List.rev rs.frames in
      let gif = GIF.from_images frames in
      let filename = now_string () ^ ".gif" in
      GIF.to_file gif filename;
      Printf.printf "Animation saved as %s\n%!" filename;
      recording_state := None

let record_frame (recording_state : recording_state_t option ref)
    (screen : Screen.t) (fb : Framebuffer.t) =
  match !recording_state with
  | None -> ()
  | Some rs ->
      if rs.current_frame >= rs.frames_to_record then
        stop_recording recording_state
      else (
        if Palette.size (Screen.palette screen) > 256 then
          failwith "GIF only supports up to 256 colors";
        let frame = capture_frame screen fb in
        recording_state :=
          Some
            {
              rs with
              frames = frame :: rs.frames;
              current_frame = rs.current_frame + 1;
            };
        if rs.current_frame + 1 = rs.frames_to_record then
          stop_recording recording_state)
