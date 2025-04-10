open Claudius
open Screenshot

let () =
  let width, height = 100, 100 in
  let scale = 2 in 

  let test_palette name palette =
    let size = Palette.size palette in
    Printf.printf "Testing palette: %s (size = %d)\n%!" name size;

    (* Generate framebuffer with values clamped to [0, palette_size - 1] *)
    let fb = Framebuffer.init (width, height) (fun x y ->
      let raw = (x * y + x + y) in
      let clamped = raw mod size in
      assert (clamped >= 0 && clamped < size);
      clamped
    ) in

    Framebuffer.set_dirty fb;
    save_screenshot ~scale fb palette
  in

  test_palette "vapourwave" (Palette.generate_vapourwave_palette 64);
  test_palette "vga" (Palette.generate_microsoft_vga_palette ());
  test_palette "monopalette" (Palette.generate_mono_palette 256);

flush_all()