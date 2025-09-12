(** Module for handling screenshots *)

val save_screenshot : Screen.t -> Framebuffer.t -> (string, string) result
(** [save_screenshot screen framebuffer] saves a screenshot with a timestamped
    filename like "screenshot_DDMMYY_HHMMSS.gif" for uniqueness. The output
    image is scaled by screen's scale factor factor Prevents multiple
    screenshots if F2 is held down. *)
