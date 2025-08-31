type t
(** Abstract type representing a loaded picture *)

val load : string -> t
(** [load filename] loads a PNG file and returns a picture. *)

val dimensions : t -> int * int
(** [dimensions pic] returns the width and height of the image in pixels. *)

val pixels : t -> int array
(** [pixels pic] returns the indexed pixel array. The pixels are arranged in
    consecutive rows, with the top left pixel of the image first. *)

val palette : t -> Palette.t
(** [palette pic] returns the color palette of the picture. *)

val with_palette_offset : t -> int -> t
(** [with_palette_offset pic offset] returns a new picture with all pixel
    indices shifted by [offset]. Palette is unchanged. *)
