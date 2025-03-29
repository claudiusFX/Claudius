
type t = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : Palette.t ;
  font            : Font.t option;
}


let create (width : int) (height : int) (scale : int) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = None}

let update (screen : t) (new_palette : Palette.t) : t =
{screen with palette = new_palette}

let create_with_font (width : int) (height : int) (scale : int) (font : Font.t) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = Some font }

let dimensions (screen : t) : int * int =
  screen.width, screen.height

let palette (screen : t) : Palette.t =
  screen.palette

let font (screen : t) : Font.t option =
  screen.font

let scale (screen : t) : int =
  screen.scale
