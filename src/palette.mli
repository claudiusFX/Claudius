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
(** [generate_plasma_palette size] Will generate a plasma colour palette with [size] number of entries. Raises
[Invalid_argument] if palette size is zero or less. *)        

val generate_linear_palette : int -> int -> int -> t
(** [generate_linear_palette color1 color2 size] returns a palette (of type [t])
that linearly interpolates between [color1] and [color2] over [size] entries.
Raises [Invalid_argument] if [size] is less than or equal to zero. *)
    
val generate_vapourwave_palette : int -> t
 (** [generate_vapourwave_palette size] returns a vapourwave palette with [size] entries.
 Internally, it calls [generate_linear_palette] using pastel purple (0x7f3b8f) 
 and pastel cyan (0x80cfcf) as endpoints.
 Raises [Invalid_argument] if [size] is less than or equal to zero. *)

val generate_microsoft_vga_palette : unit -> t
(** [generate_microsoft_vga_palette ()] returns the Microsoft VGA 16-color palette,
    as defined by sources such as Lospec and Wikipedia. *)

val generate_classic_vga_palette : unit -> t
(** [generate_classic_vga_palette ()] returns the classic IBM VGA 16-color palette,
    based on traditional values. *)

val generate_sweetie16_palette : unit -> t
    (** [generate_sweet16_palette ()] returns the Sweet16 color palette as defined by the widely recognized Lospec palette  by GrafxKid, found on Lospec:
     https://lospec.com/palette-list/sweetie-16*) 

val generate_mac_palette : unit -> t
(** [generate_mac_palette ()] returns the Macintosh 16-color palette,
    as defined by sources such as Wikipedia. *)

val load_tic80_palette: string -> t
(** [load_tic80_palette str] Will take a string [str] of the form found in TIC80 save files and load it as a palette.Raises
    [Invalid_argument] if palette size is zero or less, or if the data string is not correct. *)

val load_lospec_palette : string -> t
(** [load_lospec_palette str] Loads a palette from a Lospec-style HEX string (with or without leading #).
    Raises [Invalid_argument] if no valid colors are found. *)

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

val circle_palette: t -> int -> int32
(**[circle_palette pal offset] returns a new palette with entries rotated to offset*)

val updated_entry : t -> int -> int32 -> t
(** [updated_entry pal index new_color] checks for the index then returns a new palette with the entry at [index] updated to [new_color]. *)
