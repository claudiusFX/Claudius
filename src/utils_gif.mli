open Giflib

val color_table_of_palette : Palette.t -> ColorTable.t
val pad_palette_to_power_of_two : ColorTable.t -> ColorTable.t
val capture_frame : Screen.t -> Framebuffer.t -> Image.t