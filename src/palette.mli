(** Claudius works with colour palettes, as per computers of old. This module
lets you load and manipulate palettes. The palettes can be thought of simply
as indexed arrays of RGB values, and you write index values to the Framebuffer
rather than RGB values directly. *)

type t

(** {1 Initializations} *)

val generate_mono_palette: int -> t
(** [generate_mono_palette size] Will generate a grayscale palette going from black to white with [size] number of entries. Raises
    [Invalid_argument] if palette size is zero or less. *)

val generate_plasma_palette: int -> t
(** [generate_plasma_palette size] Will generate a plasma colour palette with [size] number of entries. Raisesif palette size is zero or less. *)

val generate_microsoft_vga_palette : unit -> t
    (** [generate_vga_palette ()] returns the Microsoft VGA 16-color palette, as defined by the widely recognized Lospec palette. *)

val generate_sweet16_palette : unit -> t
    (** [generate_sweet16_palette ()] returns the Sweet16 color palette as defined by the widely recognized Lospec palette. *) 

val load_tic80_palette: string -> t
(** [load_tic80_palette str] Will take a string [str] of the form found in TIC80 save files and load it as a palette.Raises
    [Invalid_argument] if palette size is zero or less, or if the data string is not correct. *)

(** {1 Conversion} *)

val to_list: t -> int list
(** [to_list palette] Converts the provided [palette] to a list of 24bpp RGB entries. *)

val of_list: int list -> t
(** [of_list list] Converts the provided [list] of 24bpp RGB entries to a palette. Raises
    [Invalid_argument] if list size is zero. *)

(** {1 Usage} *)

val size: t -> int
(** [size palette] Returns the number of entries in the palette. *)

val index_to_rgb: t -> int -> int32
(** [index_to_rgb palette index] Will return the 24bpp RGB prepesentation of a [palette] entry at position [index]. As per other fantasy console systems, the index value will be wrapped if it is above or below the palette size. *)
