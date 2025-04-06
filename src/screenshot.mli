(** Module for handling screenshots *)

val save_screenshot : Framebuffer.t -> Palette.t -> unit
(** [save_screenshot framebuffer palette] saves a screenshot of the current framebuffer
    as a GIF file, using the given palette for colors. The screenshot is saved with a
    timestamped filename like "screenshot_MMDDYY_HHMMSS.gif" for uniqueness. *)

val color_table_of_palette : Palette.t -> Giflib.ColorTable.t
(** [color_table_of_palette palette] converts a Claudius palette into a Giflib color table,
    which is needed to write GIFs with correct colors. *)