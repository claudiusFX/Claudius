open Giflib
open Utils_gif

type recording_state_t = {
  frames : Image.t list;
  frames_to_record : int;
  current_frame : int;
}

let max_frames_default = 500

let start_recording ?(max_frames = max_frames_default) (n : int) : recording_state_t =
      if max_frames <= 0 then failwith "Number of frames must be positive";
      if n <= 0 then failwith "Number of frames must be positive";
      if n > max_frames then
        failwith (Printf.sprintf "Maximum %d frames allowed" max_frames_default);
      Printf.printf "Started recording %d frames\n%!" n;
      { frames = []; frames_to_record = n; current_frame = 0 }

let stop_recording (recording_state : recording_state_t) : unit  =
      let frames = List.rev recording_state.frames in
      let gif = GIF.from_images frames in
      let filename = timestamp "animation" ^ ".gif" in
      GIF.to_file gif filename;
      Printf.printf "Animation saved as %s\n%!" filename

let record_frame (recording_state : recording_state_t)
    (screen : Screen.t) (fb : Framebuffer.t) : recording_state_t option =
        if Palette.size (Screen.palette screen) > 256 then
          failwith "GIF only supports up to 256 colors";
        let frame = capture_frame screen fb in
        let updated_state =
            {
              recording_state with
              frames = frame :: recording_state.frames;
              current_frame = recording_state.current_frame + 1;
            }
        in 
        if updated_state.current_frame = updated_state.frames_to_record then (
          stop_recording updated_state;
          None 
          ) else 
          Some updated_state
