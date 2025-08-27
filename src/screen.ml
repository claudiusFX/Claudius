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

  let pictures, all_palettes =
    let init_offset = Palette.size palette in
    let init_acc = ([], init_offset, [palette]) in
    let pics_rev, _, palettes_rev =
      List.fold_left
        (fun (pics_acc, offset_acc, palettes_acc) filename ->
          let pic = Picture.load filename in
          let shifted = Picture.with_palette_offset pic offset_acc in
          let next_offset = offset_acc + Palette.size (Picture.palette pic) in
          (shifted :: pics_acc, next_offset, Picture.palette pic :: palettes_acc))
        init_acc image_filenames
    in
    (List.rev pics_rev |> Array.of_list, List.rev palettes_rev)
  in

  let final_palette = Palette.concat all_palettes in

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
