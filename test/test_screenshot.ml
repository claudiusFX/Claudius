open OUnit2
open Claudius

let width, height = 100, 100
let scale = 2

let test_palette name palette =
  name >:: (fun _ ->
    let size = Palette.size palette in
    Printf.printf "Testing palette: %s (size = %d)\n%!" name size;

    (* Generate framebuffer with values clamped to [0, palette_size - 1] *)
    let fb = Framebuffer.init (width, height) (fun x y ->
      let raw = (x * y + x + y) in
      raw mod (Palette.size palette)
    ) in

    Framebuffer.set_dirty fb;
    let screen = Screen.create width height scale palette in
    Screenshot.save_screenshot [(Event.KeyDown Key.F2)] screen fb
  )

let test_palette_too_big _ =
  let palette = Palette.generate_mono_palette 300 in          (* > 256 entries *)
  let fb = Framebuffer.init (width, height) (fun _ _ -> 42) in
  Framebuffer.set_dirty fb;
  let screen = Screen.create width height scale palette in
  assert_raises
    (Failure "GIF only supports up to 256 colors")
    (fun () -> Screenshot.save_screenshot [(Event.KeyDown Key.F2)] screen fb)

let test_palette_too_big_no_press _ =
  let palette = Palette.generate_mono_palette 300 in          (* > 256 entries *)
  let fb = Framebuffer.init (width, height) (fun _ _ -> 42) in
  Framebuffer.set_dirty fb;
  let screen = Screen.create width height scale palette in
  Screenshot.save_screenshot [] screen fb

let () =
let suite = "screenshot_tests" >::: [
  test_palette "vapourwave" (Palette.generate_vapourwave_palette 64);
  test_palette "vga" (Palette.generate_microsoft_vga_palette ());
  test_palette "monopalette" (Palette.generate_mono_palette 256);
  "raises error when palette exceeds 256 colors" >:: test_palette_too_big;
  "raises error when palette exceeds 256 colors no press" >:: test_palette_too_big_no_press;
] in

run_test_tt_main suite;
