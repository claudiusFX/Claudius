open Claudius
open OUnit2

let test_basic_palette_of_ints _ =
  let cols = [0x000000 ; 0xFF0000 ; 0x00FF00 ; 0x0000FF ; 0xFFFFFF] in
  let pal = Palette.of_list cols in
  assert_equal ~msg:"Palette size" (List.length cols) (Palette.size pal);
  List.iteri (fun i c ->
    let v = Palette.index_to_rgb pal i in
    assert_equal ~msg:"Colour match" (Int32.of_int c) v
  ) cols;
  let rev = Palette.to_list pal in
  assert_equal ~msg:"Back to ints" cols rev

let test_generate_mac_palette_creation _ =
  let pal = Palette.generate_mac_palette () in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  
let test_generate_sweetie16_palette _ =
  let pal = Palette.generate_sweetie16_palette () in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  
let test_generate_linear_palette _ =
  let color1 = 0x7f3b8f in    (* Pastel purple *)
  let color2 = 0x80cfcf in    (* Pastel cyan *)
  let size = 16 in
  let pal = Palette.generate_linear_palette color1 color2 size in
  assert_equal ~msg:"Palette length" size (Palette.size pal);
  let first_color = Int32.to_int (Palette.index_to_rgb pal 0) in
  assert_equal ~msg:"First color should be color1" color1 first_color
  let last_color = Int32.to_int (Palette.index_to_rgb pal (size - 1)) in
  assert_equal ~msg:"Last color should be color2" color2 last_color
  
let test_generate_vapour_wave_creation _ =
  let size = 16 in
  let pal = Palette.generate_vapourwave_palette size in
  assert_equal ~msg:"Palette length" size (Palette.size pal);
  let pastel_purple = 0x7f3b8f in
  let pastel_cyan   = 0x80cfcf in
  let first_color = Int32.to_int (Palette.index_to_rgb pal 0) in
  let last_color  = Int32.to_int (Palette.index_to_rgb pal (size - 1)) in
  assert_equal ~msg:"First color should be pastel purple" pastel_purple first_color
  assert_equal ~msg:"Last color should be pastel cyan" pastel_cyan last_color
  let mid_color = Int32.to_int (Palette.index_to_rgb pal (size / 2)) in
  assert_bool "Mid color is distinct" (mid_color <> pastel_purple && mid_color <> pastel_cyan)
  
let test_generate_classic_vga_palette_creation _ =
  let pal = Palette.generate_classic_vga_palette () in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  
let test_generate_microsoft_vga_palette_creation _ =
  let pal = Palette.generate_microsoft_vga_palette () in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  
let test_plasma_palette_creation _ =
  let pal = Palette.generate_plasma_palette 16 in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  (* weak sauce perhaps, but just check there's no black/white here, i.e., not a mono palette *)
  List.iter (fun c ->
    assert_bool "Colour not black" (0x000000 != c);
    assert_bool "Colour not white" (0xFFFFFF != c);
  ) (Palette.to_list pal)

let test_mono_palette_creation _ =
  let pal = Palette.generate_mono_palette 16 in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  assert_equal ~msg:"Start with black" Int32.zero (Palette.index_to_rgb pal 0);
  assert_equal ~msg:"Wrap around to black" Int32.zero (Palette.index_to_rgb pal 16);
  (* I originally tested that we ended on white, but due to rounding errors we might be slightly off *)
  List.iter (fun c ->
    let r = c land 0xFF
    and g = (c lsr 8) land 0xFF
    and b = (c lsr 16) land 0xFF in
    assert_equal ~msg:"R equals G" r g;
    assert_equal ~msg:"R equals B" r b;
  ) (Palette.to_list pal)

let test_create_empty_palette_from_list _ =
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.of_list [])

let test_create_empty_plasma _ =
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.generate_plasma_palette 0);
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.generate_plasma_palette (-1))

let test_create_empty_mono _ =
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.generate_mono_palette 0);
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.generate_mono_palette (-1))

let test_load_tic80_palette _ =
  let cols = [0x000000 ; 0xFF0000 ; 0x00FF00 ; 0x0000FF ; 0xFFFFFF] in
  let pal = Palette.load_tic80_palette "000:000000FF000000FF000000FFFFFFFF" in
  assert_equal ~msg:"Palette size" (List.length cols) (Palette.size pal);
  List.iteri (fun i c ->
    let v = Palette.index_to_rgb pal i in
    assert_equal ~msg:"Colour match" (Int32.of_int c) v
  ) cols

let test_fail_with_invalid_palette_byte_count _ =
  assert_raises (Invalid_argument "String size not a multiple of 6 chars per colour") (fun _ -> Palette.load_tic80_palette "000:000000FF000000FF000000FFFFFFF")

let test_fail_load_empty_tic80_palette _ =
  assert_raises (Invalid_argument "Palette size must not be zero or negative") (fun _ -> Palette.load_tic80_palette "000:")

let test_palette_wrap_around _ =
  let cols = [0x000000 ; 0xFF0000 ; 0x00FF00 ; 0x0000FF ; 0xFFFFFF] in
  let pal = Palette.of_list cols in
  for idx = -5 to -1 do
    let v = Palette.index_to_rgb pal idx in
    let c = List.nth cols (idx + 5) in
    assert_equal ~msg:"Colour match" (Int32.of_int c) v
  done;
  for idx = 5 to 9 do
    let v = Palette.index_to_rgb pal idx in
    let c = List.nth cols (idx - 5) in
    assert_equal ~msg:"Colour match" (Int32.of_int c) v
  done


let suite =
  "PaletteTests" >::: [
    "Test simple palette set up" >:: test_basic_palette_of_ints ;
    "Test generate mac palette" >:: test_generate_mac_palette_creation;
    "Test generate sweetie16 palette" >:: test_generate_sweetie16_palette;
    "Test linear palette" >:: test_generate_linear_palette;
    "Test vapour wave creation" >:: test_generate_vapour_wave_creation;
    "Test classic vga palette creation" >:: test_generate_classic_vga_palette_creation;
    "Test microsoft vga palette creation" >:: test_generate_microsoft_vga_palette_creation;
    "Test plasma palette creation" >:: test_plasma_palette_creation ;
    "Test mono creation" >:: test_mono_palette_creation ;
    "Test fail to make empty palette" >:: test_create_empty_palette_from_list ;
    "Test fail to make zero entry plasma palette" >:: test_create_empty_plasma ;
    "Test fail to make zero entry mono palette" >:: test_create_empty_mono ;
    "Test load tic80 palette string" >:: test_load_tic80_palette ;
    "Test fail invalid tic80 palette" >:: test_fail_with_invalid_palette_byte_count ;
    "Test fail empty tic80 palette" >:: test_fail_load_empty_tic80_palette ;
    "Test palette wrap around" >:: test_palette_wrap_around ;
  ]

let () =
  run_test_tt_main suite
