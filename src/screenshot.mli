(** Module for handling screenshots *)

val save_screenshot : Event.t list -> Screen.t -> Framebuffer.t -> unit
(** [save_screenshot keys screen framebuffer] saves a screenshot with a
    timestamped filename like "screenshot_DDMMYY_HHMMSS.gif" for uniqueness. The
    output image is scaled by screen's scale factor factor Prevents multiple
    screenshots if F2 is held down. *)
