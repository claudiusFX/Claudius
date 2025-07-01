(** Module for handling animation recording *)

type recording_state_t = {
  frames : Giflib.Image.t list;
  frames_to_record : int;
  current_frame : int;
}

val start_recording :
  ?max_frames:int -> recording_state_t option ref -> int -> unit
(** [start_recording ?max_frames state n] starts recording an animation with [n]
    frames. Raises [Failure] if already recording, if n is non-positive or if
    exceeding [max_frames]. *)

val stop_recording : recording_state_t option ref -> unit
(** [stop_recording ()] stops the current recording and saves the animation.
    Raises [Failure] if not recording. *)

val record_frame :
  recording_state_t option ref -> Screen.t -> Framebuffer.t -> unit
(** [record_frame screen framebuffer] records a single frame if recording is
    active. Automatically stops recording when the requested number of frames is
    reached. Raises [Failure] if the palette has more than 256 colors. *)

val max_frames_default : int
(** Default upper bound for number of frames to be recorded when recording is
    started. *)
