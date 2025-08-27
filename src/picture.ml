open Image

type t = { palette : Palette.t; pixels : int array; width : int; height : int }

(* let global_palette : int array = Array.make 256 0
let picture_offsets : (int, int) Hashtbl.t = Hashtbl.create 16
let next_palette_offset : int ref = ref 1 *)

(* To ensure a picture’s palette is assigned an offset in the global palette. *)
(* let ensure_palette_offset (pic : t) : int =
  (* Hash based on palette contents + dimensions, so same picture reuses offset. *)
  let pal_list = Palette.to_list pic.palette in
  let pid = Hashtbl.hash (pal_list, pic.width, pic.height) in
  match Hashtbl.find_opt picture_offsets pid with
  | Some offset -> offset
  | None ->
      let count = List.length pal_list in
      let offset = !next_palette_offset in
      if offset + count > Array.length global_palette then
        failwith "Global palette overflow in Picture.ensure_palette_offset";

      List.iteri (fun i rgb24 -> global_palette.(offset + i) <- rgb24) pal_list;

      Hashtbl.add picture_offsets pid offset;

      next_palette_offset := offset + count;
      offset *)

let load_png_as_indexed (filepath : string) : Palette.t * int array * int * int
    =
  let img = ImageLib_unix.openfile filepath in
  let w = img.width in
  let h = img.height in

  let pixels_rgba =
    Array.init (w * h) (fun idx ->
        let x = idx mod w in
        let y = idx / w in
        match img.pixels with
        | RGB (r, g, b) ->
            let red = Pixmap.get r x y in
            let green = Pixmap.get g x y in
            let blue = Pixmap.get b x y in
            (red, green, blue, 255)
            (* 255 means fully opaque *)
        | RGBA (r, g, b, a) ->
            let red = Pixmap.get r x y in
            let green = Pixmap.get g x y in
            let blue = Pixmap.get b x y in
            let alpha = Pixmap.get a x y in
            (red, green, blue, alpha)
        | Grey p ->
            let g = Pixmap.get p x y in
            (g, g, g, 255)
        | GreyA (p, a) ->
            let g = Pixmap.get p x y in
            let alpha = Pixmap.get a x y in
            (g, g, g, alpha))
  in

  let module ColorMap = Map.Make (struct
    type t = int * int * int

    let compare = compare
  end) in
  let palette_map, palette_list, _ =
    Array.fold_left
      (fun (map, lst, idx) (r, g, b, a) ->
        if a = 0 then (map, lst, idx) (* transparent pixel *)
        else if ColorMap.mem (r, g, b) map then (map, lst, idx)
        else (ColorMap.add (r, g, b) idx map, lst @ [ (r, g, b) ], idx + 1))
      (ColorMap.empty, [], 1) (* index 0 is being used for transparency *)
      pixels_rgba
  in

  let palette_rgb_24 =
    0x000000
    :: List.map (fun (r, g, b) -> (r lsl 16) lor (g lsl 8) lor b) palette_list
  in

  let pal = Palette.of_list palette_rgb_24 in

  let indexed_pixels =
    Array.map
      (fun (r, g, b, a) ->
        if a = 0 then 0 else ColorMap.find (r, g, b) palette_map)
      pixels_rgba
  in

  (pal, indexed_pixels, w, h)

(* Public API, so real img data isn't tampered *)

let load (filepath : string) : t =
  let palette, pixels, w, h = load_png_as_indexed filepath in
  { palette; pixels; width = w; height = h }

let original_width (pic : t) = pic.width
let original_height (pic : t) = pic.height
let pixels (pic : t) = pic.pixels
let palette (pic : t) = pic.palette

let with_palette_offset (pic : t) (offset : int) : t =
  let shifted_pixels =
    Array.map (fun idx -> if idx = 0 then 0 else idx + offset) pic.pixels
  in
  { pic with pixels = shifted_pixels }
