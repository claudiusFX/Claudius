type t = {
  title : string;
  width : int;
  height : int;
  scale : int;
  mutable palette : Palette.t;
  font : Font.t;
  mutable dirty : bool;
  pictures : Picture.t array;
}

let create ?font ?(image_filenames = []) ?(scale = 1) title (width, height)
    palette =
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
    let init_acc = ([], init_offset, [ palette ]) in
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
    title;
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

let title t = t.title
let dimensions t = (t.width, t.height)
let palette t = t.palette
let font t = t.font
let scale t = t.scale
let is_dirty t = t.dirty
let clear_dirty t = t.dirty <- false
let pictures t = t.pictures
