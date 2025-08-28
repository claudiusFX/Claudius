(** Provides the simulated framebuffer for Claudius.

    The framebuffer is logically an array of memory in which you draw using
    palette entries, similar to say how VGA worked on old PCs. *)

type t = { data : int array array; mutable dirty : bool }

(** {1 Initializations} *)

val init : int * int -> (int -> int -> int) -> t
(** [init width height f] Creates a new framebuffer of the specified size
    [width] x [height] and initialises each pixel using the provided function.
    The function is provided the x and y coordinates of the pixel and should
    return the colour there. *)

(** {1 Drawing operations}
    These operations let you modify the framebuffer with basic shapes. This list
    isn't exhaustive, but just some basics to let people get up and running.
    Shapes will be automatically clipped to fit the framebuffer. Position (0, 0)
    is in the top left of the screen. *)

(** {2 Functional style}
    In functional style, you should aim to build up a transformation pipeline
    where you effectively have a series of steps like so:

    + Generate data
    + Advance data by time step t
    + Transform data to list of rendering Primitive.t elements
    + Pass list to Frambuffer.render *)

val render : t -> Primitives.t list -> unit
(** [render framebuffer primitives] Takes a list of primative shapes and calls
    the appropriate drawing operation for each in turn to render them into the
    provide framebuffer. *)

(** {2 Imperative style}
    As an alternative to the functional style above, you can just draw directly
    to the framebuffer in an imperative style, one operation at a time. *)

val draw_line : int -> int -> int -> int -> int -> t -> unit
(** [draw_line x0 y0 x1 y1 colour framebuffer] Draws a line between ([x0], [y0])
    ([x1], [y1]) specified in the specified [colour] into [framebuffer]. *)

val draw_circle : int -> int -> float -> int -> t -> unit
(** [draw_circle x0 y0 radius colour framebuffer] Draws the outline of a circle
    centred at ([x0], [y0]) with the specified [radius] in the specified
    [colour] into [framebuffer]. *)

val filled_circle : int -> int -> float -> int -> t -> unit
(** [filled_circle x0 y0 radius colour framebuffer] Draws a filled circle
    centred at ([x0], [y0]) with the specified [radius] in the specified
    [colour] into [framebuffer]. *)

val draw_ellipse : int -> int -> float -> float -> int -> t -> unit
(** [draw_ellipse x0 y0 a b colour framebuffer] Draws the outline of an ellipse
    centred at ([x0], [y0]) with horizontal radius [a] and vertical radius [b]
    in the specified [colour] into [framebuffer]. *)

val filled_ellipse : int -> int -> float -> float -> int -> t -> unit
(** [filled_ellipse x0 y0 a b colour framebuffer] Draws a filled ellipse centred
    at ([x0], [y0]) with horizontal radius [a] and vertical radius [b] in the
    specified [colour] into [framebuffer]. *)

val draw_rect : int -> int -> int -> int -> int -> t -> unit
(** [draw_rect x y width height colour framebuffer] Draws the outline of a
    rectangle aligned with the window, with the top left at ([x], [y]) and size
    of ([width], [height]) in the specified [colour] into [framebuffer]. *)

val filled_rect : int -> int -> int -> int -> int -> t -> unit
(** [filled_rect x y width height colour framebuffer] Draws a filled rectangle
    aligned with the window, with the top left at ([x], [y]) and size of
    ([width], [height]) in the specified [colour] into [framebuffer]. *)

val draw_triangle : int -> int -> int -> int -> int -> int -> int -> t -> unit
(** [draw_triangle x0 y0 x1 y1 x2 y2 colour framebuffer] Draws the outline of a
    triangle made from the points ([x0], [y0]), ([x1], [y1]), and ([x2], [y2])
    in the specified [colour] into [framebuffer]. *)

val filled_triangle : int -> int -> int -> int -> int -> int -> int -> t -> unit
(** [filled_triangle x0 y0 x1 y1 x2 y2 colour framebuffer] Draws a filled
    triangle made from the points ([x0], [y0]), ([x1], [y1]), and ([x2], [y2])
    in the specified [colour] into [framebuffer]. *)

val draw_polygon : (int * int) list -> int -> t -> unit
(** [draw_polygon points colour framebuffer] Draws the outline of a polygon made
    from the list of [points] in the specified [colour] into [framebuffer]. *)

val filled_polygon : (int * int) list -> int -> t -> unit
(** [filled_polygon points colour framebuffer] Draws a filled polygon made from
    the list of [points] in the specified [colour] into [framebuffer]. *)

val draw_picture : Picture.t -> ?scale:float -> int -> int -> t -> unit
(** [draw_picture pic ?scale x y buffer] draws [pic] onto [buffer] at position
    (x, y). The picture is scaled uniformly by [scale], which defaults to 1.0 if
    omitted. *)

val draw_char : int -> int -> Font.t -> char -> int -> t -> int
(** [draw_char x y font c colour framebuffer] Draws a single character [c] in
    the specified [colour] using [font]. The top left of the charcter is the
    point specified by position ([x], [y]). The return value is the number of
    pixels wide the character was. *)

val draw_string : int -> int -> Font.t -> string -> int -> t -> int
(** [draw_string x y font s colour framebuffer] Draws the string [s] in the
    specified [colour] using [font]. The top left of the first charcter is the
    point specified by position ([x], [y]). The return value is the number of
    pixels wide the string was. *)

(** {1 Pixel operations}
    Rather than working with shape primatives, sometimes you want to work with
    pixels directly, for example creating plasma effects or drawing fractals,
    etc. Again, Claudius has both a functional approach for this and an
    imperative approach. *)

(** {2 Functional style}
    In the functional style you can pass functions that will be called per pixel
    on the source, doing a full transform of the framebuffer to a new state.
    Some common tricks that emerge from this are examples like clearing the
    screen:

    {[
      Framebuffer.map_inplace (fun _ -> 0) existing_framebuffer
    ]}

    Or to fade out the previous frame (assuming you have a palette where lower
    values are darker):

    {[
      Framebuffer.map_inplace
        (fun x -> match pixel with 0 -> 0 | x -> x - 1)
        existing_framebuffer
    ]}

    All of these functions have a inplace and non_implace version: the
    non-implace version makes for more functional code, but if you do it a lot
    the memory allocations will slow things down, and in which case you may wish
    to consider using inplace versions. *)

type shader_func = int -> int
(** You pass [shader_func value] to the map functions, which call it passing in
    a single pixel value from which to derive a new pixel value. *)

type shaderi_func = int -> int -> t -> int
(** You pass [shaderi_func] to the mapi functions, which passes in both the
    current x y coordinates and the existing framebuffer from which you can then
    extract values for generating the updated value. *)

val map : shader_func -> t -> t
(** [map f framebuffer] Generates a new framebuffer of the same dimensions by
    applying the provided function [f] to each pixel value in the original to
    generate a new pixel in the target. *)

val mapi : shaderi_func -> t -> t
(** [mapi f framebuffer] Generates a new framebuffer of the same dimensions by
    applying the provided function [f] to each pixel value and its coordinates
    in the original to generate a new pixel in the target. *)

val map_inplace : shader_func -> t -> unit
(** [map_inplace f framebuffer] Updates a framebuffer by applying the provided
    function [f] to each pixel value to update its value. *)

val mapi_inplace : shaderi_func -> t -> unit
(** [mapi_inplace f framebuffer] Updates a framebuffer by applying the provided
    function [f] to each pixel value and its coordinate value to update its
    value. *)

val map2 : (int -> int -> int) -> t -> t -> t
(** [map2 f first second] Takes two framebuffers of equal size and applys the
    function [f] to each pixel pair in turn to generate a new framebuffer. *)

val map2_inplace : (int -> int -> int) -> t -> t -> unit
(** [map2_inplace f first second] Takes two framebuffers of equal size and
    applys the function [f] to each pixel pair in storing the result back in the
    first provided framebuffer. *)

(** {2 Imperative style}
    Sometimes you don't want to transform the entire framebuffer, and so you can
    do per pixel calls also. *)

val pixel_write : int -> int -> int -> t -> unit
(** [pixel_write x y colour framebuffer] Set the pixel at the specified
    coordinate to palette colour in the provided framebuffer. If the coordinate
    is outside the framebuffer then nothing is drawn, but there is no error. *)

val pixel_read : int -> int -> t -> int option
(** [pixel_read x y framebuffer] Get the pixel colour at the specified
    coordinate in the provided framebuffer. If the coordinate is outside the
    framebuffer you get None, otherwise Some colour. *)

(** {1 Misc} *)

val to_array : t -> int array array
(** [to_array framebuffer] converts the framebuffer into a 2D array. The top
    level array is an array of rows, and each row is an array of palette entry
    colours. *)

(** {2 Dirty-bit support} *)

val is_dirty : t -> bool
(** [is_dirty framebuffer] returns [true] if the framebuffer has been marked as
    'dirty'. *)

val set_dirty : t -> unit
(** [set_dirty framebuffer] marks the framebuffer as dirty. *)

val clear_dirty : t -> unit
(** [clear_dirty framebuffer] resets the dirty bit to false. *)
