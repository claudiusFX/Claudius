type t
(** Abstract type representing a loaded picture *)

val load : string -> float -> t
(** [load filename scale] loads a PNG file and returns a picture scaled
    uniformly by [scale]. Aspect ratio is preserved. *)

val set_scale : t -> float -> t
(** [set_scale pic s] returns a new picture with the scale factor updated to
    [s]. *)

val scaled_width : t -> int
(** [scaled_width pic] returns the width in pixels after applying the scale. *)

val scaled_height : t -> int
(** [scaled_height pic] returns the height in pixels after applying the scale.
*)

val original_width : t -> int
(** [original_width pic] returns the original width in pixels. *)

val original_height : t -> int
(** [original_height pic] returns the original height in pixels. *)

val scale : t -> float
(** [scale pic] returns the scale factor. *)

val pixels : t -> int array
(** [pixels pic] returns the indexed pixel array. *)

val palette : t -> Palette.t
(** [palette pic] returns the color palette of the picture. *)
