(** Common utilies for screenshot and animation handling *)

open Giflib

val timestamp : string -> string
(** [timestamp prefix] returns a filename with current timestamp *)

val color_table_of_palette : Palette.t -> ColorTable.t
(** [color_table_of_palette palette] converts a palette to a GIF color table. *)

val pad_palette_to_power_of_two : ColorTable.t -> ColorTable.t
(** [pad_palette_to_power_of_two table] extends a color table to the next power
    of two, padding with black as needed, up to maximum 256 colors. *)

val capture_frame : Screen.t -> Framebuffer.t -> Image.t
(** [capture_frame screen framebuffer] captures the current framebuffer contents
    as a compressed GIF image. Raises [Failure] if framebuffer contains invalid
    pixels. *)
