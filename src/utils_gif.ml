open Giflib
open Unix

let timestamp prefix =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%s_%02d%02d%02d_%02d%02d%02d" prefix (tm.tm_year mod 100)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let color_table_of_palette (p : Palette.t) : ColorTable.t =
  Array.init (Palette.size p) (fun i ->
      let rgb32 = Palette.index_to_rgb p i in
      let rgb = Int32.to_int rgb32 in
      let r = (rgb lsr 16) land 0xFF in
      let g = (rgb lsr 8) land 0xFF in
      let b = rgb land 0xFF in
      (r, g, b))

let capture_frame (screen : Screen.t) (fb : Framebuffer.t) =
  let width, height = Screen.dimensions screen in
  let scale = Screen.scale screen in
  let palette = Screen.palette screen in
  let palette_size = Palette.size palette in

  let scaled_width = width * scale in
  let scaled_height = height * scale in

  let size = scaled_width * scaled_height in

  let colors = palette |> color_table_of_palette in

  let pixels =
    Array.init size (fun idx ->
        let x = idx mod scaled_width in
        let y = idx / scaled_width in
        let src_x = x / scale in
        let src_y = y / scale in
        let v =
          match Framebuffer.pixel_read src_x src_y fb with
          | Some v -> v
          | None ->
              failwith
                (Printf.sprintf "Invalid pixel coordinate (%d,%d)" src_x src_y)
        in
        if v < 0 || v > palette_size then
          failwith
            (Printf.sprintf "Framebuffer value %d out of byte range at (%d,%d)"
               v src_x src_y);
        v)
  in

  Image.of_pixels (scaled_width, scaled_height) colors pixels
