type t = {
  width : int;
  height : int;
  scale : int;
  mutable palette : Palette.t;
  font : Font.t;
  mutable dirty : bool;
}

let create ?font (width : int) (height : int) (scale : int)
    (palette : Palette.t) : t =
  if scale <= 0 then raise (Invalid_argument "Invalid scale");
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");

  let font =
    match font with
    | Some f -> f
    | None -> (
        let default_font_data =
          match Builtins.read "TamzenForPowerline10x20.psf" with
          | Some font -> font
          | None ->
              failwith
                (Printf.sprintf "Default font file not found in builtins.")
        in
        match Font.of_bytes (Bytes.of_string default_font_data) with
        | Ok f -> f
        | Error e ->
            failwith (Printf.sprintf "Failed to load default font: %s" e))
  in

  { width; height; scale; palette; font; dirty = true }

let update_palette (screen : t) (new_palette : Palette.t) : unit =
  screen.palette <- new_palette;
  screen.dirty <- true

let create_with_font (width : int) (height : int) (scale : int) (font : Font.t)
    (palette : Palette.t) : t =
  create ~font width height scale palette

let dimensions (screen : t) : int * int = (screen.width, screen.height)
let palette (screen : t) : Palette.t = screen.palette
let font (screen : t) : Font.t = screen.font
let scale (screen : t) : int = screen.scale
let is_dirty (screen : t) : bool = screen.dirty
let clear_dirty (screen : t) : unit = screen.dirty <- false
