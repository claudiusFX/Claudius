(** Provides the font rendering for Claudius.

Doesn't render to screen - used mostly by the framebuffer draw char/string mechanisms. *)

type t
(** Type for a font *)

module Glyph : sig
  type t
  (** Type for one entry in the font *)


  val dimensions: t -> (int * int * int * int)
  (** [glyph_dimensions glyph] Returns the width and height of the specified glyph. *)

  val bitmap: t -> bytes
  (** [glyph_bitmap glyph] Renders a glyph to a series of bytes. The data is 1 bit per pixel,
      as a series of bytes per row, padded to the appropriate next byte boundary. *)

end


(** {1 Initializations} *)

val of_file: string -> (t, string) result
(** [of_file filepath] Loads a bitmap font from a PSF file, or a description
  of why the load failed. *)

val of_bytes: Bytes.t -> (t, string) result
(** [of_bytes fontdata] Loads a bitmap font from a PSF file loaded into bytes, or a description
  of why the load failed. *)

(** {1 Using} *)

val glyph_count: t -> int
(** [glyph_count font] Returns a count of how many glyphs are in the font. *)

val glyph_of_char: t -> Uchar.t -> Glyph.t option
(** [glyph_of_char font char] Gets the glyph that maps to a given character in the font,
    or None if that character doesn't have an entry. *)


(** {1 Debug} *)

val print_header: t -> unit
(** [print_header font] A utility method to dump the font's header information to stdout. *)
