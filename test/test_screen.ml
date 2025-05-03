open Claudius
open OUnit2

let test_basic_screen_creation _ =
  let palette = Palette.generate_mono_palette 2 in
  let screen = Screen.create 640 480 2 palette in
  assert_equal ~msg:"Dimensions" (640, 480) (Screen.dimensions screen);
  assert_equal ~msg:"Scale" 2 (Screen.scale screen);
  let font = Screen.font screen in
  assert_bool "Font" ((Font.glyph_count font) > 0);
  assert_equal ~msg:"Palette" palette (Screen.palette screen)

let test_fail_invalid_scale _ =
  let palette = Palette.generate_mono_palette 2 in
  assert_raises (Invalid_argument "Invalid scale") (fun _ -> Screen.create 640 480 (-1) palette)

let test_fail_invalid_dimensions _ =
  let palette = Palette.generate_mono_palette 2 in
  assert_raises (Invalid_argument "Invalid height") (fun _ -> Screen.create 10 0 2 palette);
  assert_raises (Invalid_argument "Invalid width") (fun _ -> Screen.create 0 10 2 palette);
  assert_raises (Invalid_argument "Invalid height") (fun _ -> Screen.create 10 (-10) 2 palette);
  assert_raises (Invalid_argument "Invalid width") (fun _ -> Screen.create (-10) 10 2 palette)

let test_update_palette _ =
  let initial_palette = Palette.generate_mono_palette 2 in
  let screen = Screen.create 640 480 2 initial_palette in
  Screen.clear_dirty screen;
  assert_equal ~msg:"Dirty flag should be false after clearing" false (Screen.is_dirty screen);
  let new_palette = Palette.generate_plasma_palette 2 in
   Screen.update_palette screen new_palette;
   assert_equal ~msg:"Palette should be updated" new_palette (Screen.palette screen);
   assert_equal ~msg:"Dirty flag should be true after update" true (Screen.is_dirty screen);
   Screen.clear_dirty screen;
   assert_equal ~msg:"Dirty flag should be cleared" false (Screen.is_dirty screen)

let test_non_default_font _ =
  (* the default font is the powerline version that should have more glyphs *)
  let font = match Font.of_file "../thirdparty/tamzen-font/psf/Tamzen10x20.psf" with
  | Ok f -> f
  | Error msg -> assert_failure (Printf.sprintf "failed to load font: %s" msg)
  in
  let palette = Palette.generate_mono_palette 2 in
  let screen_default = Screen.create 640 480 2 palette
  and screen_with_font = Screen.create ~font 640 480 2 palette in
  let default_font = Screen.font screen_default
  and new_font = Screen.font screen_with_font in
  assert_bool "default font not our font" (font != default_font);
  assert_bool "non-default font is our font" (font == new_font)

let suite =
  "Screen tests" >::: [
    "Test simple screen set up" >:: test_basic_screen_creation ;
    "Test fail with invalid scale" >:: test_fail_invalid_scale ;
    "Test fail with invalid dimensions" >:: test_fail_invalid_dimensions ;
    "Test update palette" >:: test_update_palette ;
    "Test loading non-default font" >:: test_non_default_font;
  ]

let () =
  run_test_tt_main suite
