open OUnit2
open Claudius
open Picture

let test_valid_png _ =
  (* Credits for tetris.png: https://publicdomainvectors.org/en/free-clipart/3D-Tetris-blocks-vector-illustration/6089.html *)
  let pic = load "../test_assets/tetris.png" in
  assert_bool "width > 0" (original_width pic > 0);
  assert_bool "height > 0" (original_height pic > 0);
  assert_bool "has pixels" (Array.length (pixels pic) > 0)

let test_scaled_dimensions _ =
  let pic = load "../test_assets/tetris.png" in
  let w = original_width pic in
  let h = original_height pic in
  assert_equal w (Array.length (pixels pic) / h)

let test_draw_picture_normal _ =
  let pic = load "../test_assets/tetris.png" in
  let pal = palette pic in
  assert_bool "palette has entries" (Palette.size pal > 0);
  assert_bool "pixels reference palette" (pixels pic |> Array.exists ((<>) 0))

let test_draw_picture_negative_offset _ =
  let pic = load "../test_assets/tetris.png" in
  let shifted = with_palette_offset pic (-1) in
  Array.iteri
    (fun i idx ->
      if idx = 0 then assert_equal 0 (pixels shifted).(i)
      else assert_equal (idx - 1) (pixels shifted).(i))
    (pixels pic)

let test_draw_picture_scaled _ =
  let pic = load "../test_assets/tetris.png" in
  let w = original_width pic in
  let h = original_height pic in
  assert_equal (w * h) (Array.length (pixels pic))

let test_load_png_as_indexed_transparent _ =
  let pic = Picture.load "../test_assets/tetris.png" in
  let pal = Picture.palette pic in
  let pixels = Picture.pixels pic in
  let w = Picture.original_width pic in
  let h = Picture.original_height pic in
  assert_bool "image has width > 0" (w > 0);
  assert_bool "image has height > 0" (h > 0);
  (* palette[0] reserved for transparency *)
  assert_equal 0x000000l (Palette.index_to_rgb pal 0);
  assert_bool "transparent pixel present" (Array.exists ((=) 0) pixels)

let test_with_palette_offset _ =
  let pic = load "../test_assets/tetris.png" in
  let shifted = with_palette_offset pic 10 in
  Array.iteri
    (fun i idx ->
      if idx = 0 then assert_equal 0 (pixels shifted).(i) (* transparency stays 0 *)
      else assert_equal (idx + 10) (pixels shifted).(i))
    (pixels pic)

let suite =
  "Picture tests"
  >::: [
         "valid_png" >:: test_valid_png;
         "scaled_dimensions" >:: test_scaled_dimensions;
         "draw_picture_normal" >:: test_draw_picture_normal;
         "draw_picture_negative_offset" >:: test_draw_picture_negative_offset;
         "draw_picture_scaled" >:: test_draw_picture_scaled;
         "load_png_as_indexed transparent" >:: test_load_png_as_indexed_transparent;
         "with_palette_offset" >:: test_with_palette_offset;
       ]

let () = run_test_tt_main suite
