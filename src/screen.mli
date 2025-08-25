(** Information about the display that can be accessed from the running code to
    work out screen size etc. The screen represents the window in which things
    will be drawn. *)

type t

(** {1 Initializations} *)

val create : ?font:Font.t -> ?image_filenames:string list -> int -> int -> int -> Palette.t -> t
(** [create font width height scale palette] Creates a new screen of the
    specified size [width] x [height], and it will be rendered in a window
    scaled up by the [scale] factor provided. The framebuffers used when running
    will be indexed into the [palette] provided here. Raises [Invalid_argument]
    if the dimensions or scale are either zero or negative. If no [font] is
    provided then a default font is used. If [image_filenames] is provided, 
    the images will be loaded and their palettes merged into the screen's 
    global palette.*)

val update_palette : t -> Palette.t -> unit
(**[update screen new_palette] creates a new screen with updated palette and
   marks the screen as dirty.*)

val create_with_font : int -> int -> int -> Font.t -> Palette.t -> t
(** [create width height scale font palette] Deprecated: now use create with the
    optional font. *)

(** {1 Access} *)

val dimensions : t -> int * int
(** [dimensions screen] Returns the width and height of the [screen]. *)

val palette : t -> Palette.t
(** [palette screen] Returns the palette associated with the [screen]. *)

val scale : t -> int
(** [scale screen] Returns the scaling factor used when drawing [screen] to a
    window. *)

val font : t -> Font.t
(** [font screen] Returns the font associated with the [screen] if one was
    provided, or [None] otherwise. *)

val is_dirty : t -> bool
(** [is_dirty screen] returns [true] if the screen is marked as dirty (needing
    redraw). *)

val clear_dirty : t -> unit
(** [clear_dirty screen] returns a new screen with the dirty flag cleared. *)

val pictures : t -> Picture.t array
(** [pictures screen] returns the array of pictures loaded into the screen. *)