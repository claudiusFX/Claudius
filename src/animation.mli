(** Module for handling animation recording *)

val start_recording : int -> unit
(** [start_recording n] starts recording an animation with n frames.
    Raises [Failure] if already recording or if n is invalid. *)

val stop_recording : unit -> unit
(** [stop_recording ()] stops the current recording and saves the animation.
    Raises [Failure] if not recording. *)

val record_frame : Screen.t -> Framebuffer.t -> unit
(** [record_frame screen framebuffer] records a single frame if recording is active.
    Automatically stops recording when the requested number of frames is reached.
    Raises [Failure] if the palette has more than 256 colors. *) 