
type t = {
  width           : int ;
  height          : int ;
  scale           : int ;
  mutable palette         : Palette.t ;
  font            : Font.t option;
  mutable dirty           : bool;
}


let create (width : int) (height : int) (scale : int) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = None; dirty = true}

  let update_palette (screen : t) (new_palette : Palette.t) : unit =
    screen.palette <- new_palette;
    screen.dirty <- true  

let create_with_font (width : int) (height : int) (scale : int) (font : Font.t) (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  { width ; height ; scale ; palette ; font = Some font; dirty = true }

let dimensions (screen : t) : int * int =
  screen.width, screen.height

let palette (screen : t) : Palette.t =
  screen.palette

let font (screen : t) : Font.t option =
  screen.font

let scale (screen : t) : int =
  screen.scale

let is_dirty (screen : t) : bool =
  screen.dirty

let clear_dirty (screen : t) : unit =
  screen.dirty <- false