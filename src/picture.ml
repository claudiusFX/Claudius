open Image

type t = {
  palette : Palette.t;
  pixels : int array;
  width : int;
  height : int;
  scale : float;
}

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

  if List.length palette_rgb_24 > 256 then
    invalid_arg
      (Printf.sprintf "PNG uses %d unique colors — exceeds the 256-color limit"
         (List.length palette_rgb_24));

  let pal = Palette.of_list palette_rgb_24 in

  let indexed_pixels =
    Array.map
      (fun (r, g, b, a) ->
        if a = 0 then 0 else ColorMap.find (r, g, b) palette_map)
      pixels_rgba
  in

  (pal, indexed_pixels, w, h)

(* Public API, so real img data isn't tampered *)
let load (filepath : string) (scale : float) : t =
  if scale <= 0.0 then invalid_arg "Picture.load: scale must be > 0";
  let palette, pixels, w, h = load_png_as_indexed filepath in
  { palette; pixels; width = w; height = h; scale }

let set_scale (pic : t) (s : float) : t =
  if s <= 0.0 then invalid_arg "Picture.set_scale: scale must be > 0";
  { pic with scale = s }

let original_width (pic : t) = pic.width
let original_height (pic : t) = pic.height
let scaled_width (pic : t) = int_of_float (float pic.width *. pic.scale)
let scaled_height (pic : t) = int_of_float (float pic.height *. pic.scale)
let scale (pic : t) = pic.scale
let palette (pic : t) = pic.palette
let pixels (pic : t) = pic.pixels
