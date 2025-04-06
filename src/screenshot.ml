open Giflib
open Unix

let now_string () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "screenshot_%02d%02d%02d_%02d%02d%02d"
    tm.tm_mday (tm.tm_mon + 1) (tm.tm_year mod 100)
    tm.tm_hour tm.tm_min tm.tm_sec

let color_table_of_palette (p : Palette.t) : ColorTable.t =
  Array.init (Palette.size p) (fun i ->
    let rgb32 = Palette.index_to_rgb p i in
    let rgb = Int32.to_int rgb32 in
    let r = (rgb lsr 16) land 0xFF in
    let g = (rgb lsr 8) land 0xFF in
    let b = rgb land 0xFF in
    (r, g, b)
  )

let pad_palette_to_power_of_two (colors : ColorTable.t) : ColorTable.t =
  let len = Array.length colors in
  let next_pow2 n =
    let rec loop x = if x >= n then x else loop (x * 2) in
    loop 1
  in
  let target_len = min 256 (next_pow2 len) in
  Array.init target_len (fun i ->
    if i < len then colors.(i) else (0, 0, 0)
  )

let save_screenshot (fb : Framebuffer.t) (palette : Palette.t) =
  let width = Array.length fb.data.(0) in
  let height = Array.length fb.data in
  Printf.printf "Framebuffer dimensions: %d x %d\n" width height;   (*Debug*)
  let size = width * height in

  let nulls_replaced = ref 0 in
  let pixels =
    Bytes.init size (fun idx ->
      let x = idx mod width in
      let y = idx / width in
      let v = fb.data.(y).(x) in
      if v < 0 || v > 255 then
        failwith (Printf.sprintf "Framebuffer value %d out of byte range at (%d,%d)" v x y);
      if v = 0 then incr nulls_replaced;
      let max_index = Palette.size palette - 1 in
      let safe_v = 
        if v = 0 then 1
        else if v > max_index then max_index
        else v in
      Char.chr safe_v
    )
  in
  Printf.printf "Replaced %d null pixels with index 1\n%!" !nulls_replaced;

  let colors = palette
    |> color_table_of_palette
    |> pad_palette_to_power_of_two
  in

  let color_depth =
    let len = Array.length colors in
    let rec bits_needed n b = if n <= 1 then b else bits_needed (n / 2) (b + 1) in
    min 8 (max 2 (bits_needed (len-1) 1))
  in

  let palette_len = Array.length colors in
  let max_allowed_color_depth =
    let rec bits_needed n b = if n <= 1 then b else bits_needed (n / 2) (b + 1) in
    bits_needed (palette_len - 1) 1
  in
  if color_depth > max_allowed_color_depth then
    failwith (Printf.sprintf
      "Color depth %d exceeds max allowed for palette size %d (max: %d)"
      color_depth palette_len max_allowed_color_depth);  

  Printf.printf "Color depth: %d\n" color_depth;   (*Debug*)
  Printf.printf "Pixels length: %d, expected: %d\n" (Bytes.length pixels) (width * height);

  Printf.printf "Palette size: %d, color depth: %d\n" (Array.length colors) color_depth;
  let compressed = Lzw.encode pixels color_depth in
  Printf.printf "Uncompressed size: %d, Compressed size: %d\n%!" (Bytes.length pixels) (Bytes.length compressed);
  
  let image = Image.v
    (width, height)
    colors
    compressed
    color_depth
    false
  in

  let null_count = ref 0 in                   (*Debug*)
  Bytes.iter (fun c -> if Char.code c = 0 then incr null_count) pixels;
  Printf.printf "Number of null bytes in pixels: %d\n%!" !null_count;

  if Bytes.length pixels <> width * height then                   (*Debug*)
    failwith (Printf.sprintf "too few/many pixels: expected %d got %d" (width * height) (Bytes.length pixels));

  let gif = GIF.from_image image in
  let filename = now_string () ^ ".gif" in
  GIF.to_file gif filename;
  Printf.printf "Screenshot saved to %s\n%!" filename
