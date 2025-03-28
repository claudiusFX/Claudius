(** Information about the display that can be accessed from the running code to work out screen size etc. The screen
    represents the window in which things will be drawn. *)

type t

(** {1 Initializations} *)

val create: int -> int -> int -> Palette.t -> t
(** [create width height scale palette] Creates a new screen of the specified size [width] x [height],
    and it will be rendered in a window scaled up by the [scale] factor provided. The framebuffers
    used when running will be indexed into the [palette] provided here. Raises [Invalid_argument] if
   the dimensions or scale are either zero or negative. *)

val update: t -> Palette.t -> t
(**[update screen new_palette] creates a new screen with updated palette*)

val create_with_font: int -> int -> int -> Font.t -> Palette.t -> t
(** [create width height scale font palette] Creates a new screen of the specified size [width] x [height],
    and it will be rendered in a window scaled up by the [scale] factor provided. A font to be
    used for future rendering of characters is provided here. The framebuffers  used when running
    will be indexed into the [palette] provided here. Raises [Invalid_argument] if
   the dimensions or scale are either zero or negative. *)

(** {1 Access} *)

val dimensions: t -> int * int
(** [dimensions screen] Returns the width and height of the [screen]. *)

val palette: t -> Palette.t
(** [palette screen] Returns the palette associated with the [screen]. *)

val scale : t -> int
(** [scale screen] Returns the scaling factor used when drawing [screen] to a window. *)

val font : t -> Font.t option
(** [font screen] Returns the font associated with the [screen] if one was provided, or [None] otherwise. *)
