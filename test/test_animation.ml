open OUnit2
open Claudius

let width, height = (100, 100)
let scale = 2

let test_basic_recording _ =
  let palette = Palette.generate_vapourwave_palette 64 in
  let fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let screen = Screen.create width height scale palette in

  (* Start recording *)
  Animation.start_recording 10;

  (* Record 10 frames *)
  for _ = 0 to 9 do
    Animation.record_frame screen fb
  done;

  (* Verify recording stopped automatically *)
  assert_raises (Failure "Not recording animation") (fun () ->
      Animation.stop_recording ())

let test_invalid_frame_count _ =
  assert_raises (Failure "Number of frames must be positive") (fun () ->
      Animation.start_recording 0);
  assert_raises (Failure "Maximum 100 frames allowed") (fun () ->
      Animation.start_recording 101)

let test_double_recording _ =
  let palette = Palette.generate_vapourwave_palette 64 in
  let _fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let _screen = Screen.create width height scale palette in

  Animation.start_recording 10;
  assert_raises (Failure "Already recording animation") (fun () ->
      Animation.start_recording 10);
  Animation.stop_recording ()

let test_palette_too_big _ =
  let palette = Palette.generate_mono_palette 300 in
  let fb = Framebuffer.init (width, height) (fun _ _ -> 42) in
  let screen = Screen.create width height scale palette in

  Animation.start_recording 10;
  assert_raises (Failure "GIF only supports up to 256 colors") (fun () ->
      Animation.record_frame screen fb);
  Animation.stop_recording ()

let suite =
  "animation_tests"
  >::: [
         "Test basic recording" >:: test_basic_recording;
         "Test invalid frame count" >:: test_invalid_frame_count;
         "Test double recording" >:: test_double_recording;
         "Test palette too big" >:: test_palette_too_big;
       ]

let () = run_test_tt_main suite
