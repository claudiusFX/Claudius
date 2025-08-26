type t = {
  width : int;
  height : int;
  scale : int;
  mutable palette : Palette.t;
  font : Font.t;
  mutable dirty : bool;
  pictures : Picture.t array;
}

let create ?font ?(image_filenames = []) (width : int) (height : int)
    (scale : int) (palette : Palette.t) : t =
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

  let pictures =
    let offset = ref (Palette.size palette) in
    List.map
      (fun filename ->
        let pic = Picture.load filename in
        let shifted = Picture.with_palette_offset pic !offset in
        offset := !offset + Palette.size (Picture.palette pic);
        shifted)
      image_filenames
    |> Array.of_list
  in

  let final_palette =
    let all_palettes =
      palette :: (Array.to_list pictures |> List.map Picture.palette)
    in
    Palette.concat all_palettes
  in

  {
    width;
    height;
    scale;
    palette = final_palette;
    font;
    dirty = true;
    pictures;
  }

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
let pictures (screen : t) : Picture.t array = screen.pictures
