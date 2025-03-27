
type t = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : Palette.t ;
  font            : Fontv2.t option;
}


let create (width : int) (height : int) (scale : int) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = None}

let create_with_font (width : int) (height : int) (scale : int) (font : Fontv2.t) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = Some font }

let dimensions (screen : t) : int * int =
  screen.width, screen.height

let palette (screen : t) : Palette.t =
  screen.palette

let font (screen : t) : Fontv2.t option =
  screen.font

let scale (screen : t) : int =
  screen.scale
