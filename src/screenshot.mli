(** Module for handling screenshots *)

val save_screenshot : Screen.t -> Framebuffer.t -> unit
(** [save_screenshot screen framebuffer] saves a screenshot of the current framebuffer.
    The screenshot is saved with a timestamped filename like "screenshot_DDMMYY_HHMMSS.gif" for uniqueness. 
    The output image is scaled by screen's scale factor factor using nearest-neighbor scaling. *)