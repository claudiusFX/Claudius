type t
(** Abstract type representing a loaded picture *)

val load : string -> t
(** [load filename] loads a PNG file and returns a picture. *)

val original_width : t -> int
(** [original_width pic] returns the original width in pixels. *)

val original_height : t -> int
(** [original_height pic] returns the original height in pixels. *)

val pixels : t -> int array
(** [pixels pic] returns the indexed pixel array. *)

val palette : t -> Palette.t
(** [palette pic] returns the color palette of the picture. *)

val with_palette_offset : t -> int -> t
(** [with_palette_offset pic offset] returns a new picture with all pixel indices
    shifted by [offset]. Palette is unchanged. *)

val ensure_palette_offset : t -> int
(** [ensure_palette_offset pic] ensures that [pic]'s palette is installed
    into the global palette, and returns the assigned offset. *)

val global_palette : int array
(** The global 256-color palette shared by all pictures. *)