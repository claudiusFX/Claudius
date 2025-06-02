open Claudius
open OUnit2

let test_basic_framebuffer_creation _ =
  let fb = Framebuffer.init (10, 20) (fun x y -> x + (y * 10)) in
  let raw = Framebuffer.to_array fb in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri
    (fun y row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iteri
        (fun x pixel -> assert_equal ~msg:"Pixel value" (x + (y * 10)) pixel)
        row)
    raw;
  for y = 0 to 19 do
    for x = 0 to 9 do
      let pixel = Framebuffer.pixel_read x y fb in
      assert_equal ~msg:"pixel read" (Some (x + (y * 10))) pixel
    done
  done

let test_fail_invalid_dimensions _ =
  assert_raises (Invalid_argument "Invalid width") (fun _ ->
      Framebuffer.init (-10, 20) (fun _ _ -> 0));
  assert_raises (Invalid_argument "Invalid height") (fun _ ->
      Framebuffer.init (10, -20) (fun _ _ -> 0))

let test_framebuffer_write_pixel _ =
  let fb = Framebuffer.init (10, 20) (fun _ _ -> 0) in
  for y = 0 to 19 do
    for x = 0 to 9 do
      Framebuffer.pixel_write x y (x + (y * 10)) fb
    done
  done;
  let raw = Framebuffer.to_array fb in
  Array.iteri
    (fun y row ->
      Array.iteri
        (fun x pixel -> assert_equal ~msg:"Pixel value" (x + (y * 10)) pixel)
        row)
    raw

let test_framebuffer_write_pixel_outside _ =
  (* Expect updates outside framebuffer to be ignored *)
  let fb = Framebuffer.init (10, 20) (fun _ _ -> 0) in
  for y = 0 to 19 do
    Framebuffer.pixel_write (-1) y 42 fb;
    Framebuffer.pixel_write 10 y 42 fb
  done;
  for x = 0 to 9 do
    Framebuffer.pixel_write x (-1) 42 fb;
    Framebuffer.pixel_write x 20 42 fb
  done;
  let raw = Framebuffer.to_array fb in
  Array.iter
    (fun row ->
      Array.iter (fun pixel -> assert_equal ~msg:"Pixel value" 0 pixel) row)
    raw

let test_basic_framebuffer_shader _ =
  let fb = Framebuffer.init (10, 20) (fun x y -> x + (y * 10)) in
  let updated = Framebuffer.map (fun x -> x + 1) fb in
  let raw = Framebuffer.to_array updated in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri
    (fun y row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iteri
        (fun x pixel ->
          assert_equal ~msg:"Pixel value updated" (1 + x + (y * 10)) pixel)
        row)
    raw;
  for y = 0 to 19 do
    for x = 0 to 9 do
      let pixel = Framebuffer.pixel_read x y updated in
      assert_equal ~msg:"pixel read updated" (Some (1 + x + (y * 10))) pixel;
      let original = Framebuffer.pixel_read x y fb in
      assert_equal ~msg:"pixel read original" (Some (x + (y * 10))) original
    done
  done

let test_basic_framebuffer_shader_inplace _ =
  let fb = Framebuffer.init (10, 20) (fun x y -> x + (y * 10)) in
  Framebuffer.map_inplace (fun x -> x + 1) fb;
  let raw = Framebuffer.to_array fb in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri
    (fun y row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iteri
        (fun x pixel ->
          assert_equal ~msg:"Pixel value" (1 + x + (y * 10)) pixel)
        row)
    raw;
  for y = 0 to 19 do
    for x = 0 to 9 do
      let pixel = Framebuffer.pixel_read x y fb in
      assert_equal ~msg:"pixel read" (Some (1 + x + (y * 10))) pixel
    done
  done

let test_basic_framebuffer_shaderi _ =
  let fb = Framebuffer.init (10, 20) (fun _ _ -> 0) in
  let updated = Framebuffer.mapi (fun x y _ -> x + (y * 10)) fb in
  let raw = Framebuffer.to_array updated in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri
    (fun y row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iteri
        (fun x pixel ->
          assert_equal ~msg:"Pixel value updated" (x + (y * 10)) pixel)
        row)
    raw;
  for y = 0 to 19 do
    for x = 0 to 9 do
      let pixel = Framebuffer.pixel_read x y updated in
      assert_equal ~msg:"pixel read updated" (Some (x + (y * 10))) pixel;
      let original = Framebuffer.pixel_read x y fb in
      assert_equal ~msg:"pixel read original" (Some 0) original
    done
  done

let test_basic_framebuffer_shaderi_inplace _ =
  let fb = Framebuffer.init (10, 20) (fun _ _ -> 0) in
  Framebuffer.mapi_inplace (fun x y _ -> x + (y * 10)) fb;
  let raw = Framebuffer.to_array fb in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri
    (fun y row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iteri
        (fun x pixel -> assert_equal ~msg:"Pixel value" (x + (y * 10)) pixel)
        row)
    raw;
  for y = 0 to 19 do
    for x = 0 to 9 do
      let pixel = Framebuffer.pixel_read x y fb in
      assert_equal ~msg:"pixel read" (Some (x + (y * 10))) pixel
    done
  done

let test_merge_framebuffers _ =
  let fb1 = Framebuffer.init (10, 20) (fun x y -> (x + y) mod 2)
  and fb2 = Framebuffer.init (10, 20) (fun x y -> 1 - ((x + y) mod 2)) in
  let merged = Framebuffer.map2 (fun a b -> a + b) fb1 fb2 in
  let raw = Framebuffer.to_array merged in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iter
    (fun row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iter (fun pixel -> assert_equal ~msg:"Pixel value" 1 pixel) row)
    raw

let test_merge_framebuffers_inplace _ =
  let fb1 = Framebuffer.init (10, 20) (fun x y -> (x + y) mod 2)
  and fb2 = Framebuffer.init (10, 20) (fun x y -> 1 - ((x + y) mod 2)) in
  Framebuffer.map2_inplace (fun a b -> a + b) fb1 fb2;
  let raw = Framebuffer.to_array fb1 in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iter
    (fun row ->
      assert_equal ~msg:"X axis size" 10 (Array.length row);
      Array.iter (fun pixel -> assert_equal ~msg:"Pixel value" 1 pixel) row)
    raw

let test_merge_mismatched_framebuffers _ =
  let fb1 = Framebuffer.init (10, 20) (fun x y -> (x + y) mod 2)
  and fb2 = Framebuffer.init (20, 10) (fun x y -> 1 - ((x + y) mod 2)) in
  assert_raises
    (Invalid_argument
       "Merging framebuffers requires both to have same dimensions") (fun _ ->
      Framebuffer.map2 (fun a b -> a + b) fb1 fb2)

let test_merge_mismatched_framebuffers_inplace _ =
  let fb1 = Framebuffer.init (10, 20) (fun x y -> (x + y) mod 2)
  and fb2 = Framebuffer.init (20, 10) (fun x y -> 1 - ((x + y) mod 2)) in
  assert_raises
    (Invalid_argument
       "Merging framebuffers requires both to have same dimensions") (fun _ ->
      Framebuffer.map2_inplace (fun a b -> a + b) fb1 fb2)

let suite =
  "Frambuffer tests"
  >::: [
         "Test simple framebuffer set up" >:: test_basic_framebuffer_creation;
         "Test fail with invalid dimensions" >:: test_fail_invalid_dimensions;
         "Test write pixel" >:: test_framebuffer_write_pixel;
         "Test write outside framebuffer"
         >:: test_framebuffer_write_pixel_outside;
         "Test simple shader" >:: test_basic_framebuffer_shader;
         "Test simple shader inplace" >:: test_basic_framebuffer_shader_inplace;
         "Test simple shaderi" >:: test_basic_framebuffer_shaderi;
         "Test simple shaderi inplace"
         >:: test_basic_framebuffer_shaderi_inplace;
         "Test simple merged" >:: test_merge_framebuffers;
         "Test simple merged inplace" >:: test_merge_framebuffers_inplace;
         "Test fail to merge mismatched framebuffers"
         >:: test_merge_mismatched_framebuffers;
         "Test fail to merge mismatched framebuffers inplace"
         >:: test_merge_mismatched_framebuffers_inplace;
       ]

let () = run_test_tt_main suite
