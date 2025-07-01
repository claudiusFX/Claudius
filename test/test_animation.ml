open OUnit2
open Claudius

let width, height = (100, 100)
let scale = 2

let test_basic_recording _ =
  let recording_state = ref None in
  let palette = Palette.generate_vapourwave_palette 64 in
  let fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let screen = Screen.create width height scale palette in

  Animation.start_recording recording_state 10;

  for _ = 0 to 9 do
    Animation.record_frame recording_state screen fb
  done;

  assert_raises (Failure "Not recording animation") (fun () ->
      Animation.stop_recording recording_state)

let test_invalid_frame_count _ =
  let recording_state = ref None in
  assert_raises (Failure "Number of frames must be positive") (fun () ->
      Animation.start_recording recording_state 0);
  let msg =
    Printf.sprintf "Maximum %d frames allowed" Animation.max_frames_default
  in
  assert_raises (Failure msg) (fun () ->
      Animation.start_recording recording_state
        (Animation.max_frames_default + 1))

let test_double_recording _ =
  let recording_state = ref None in
  let palette = Palette.generate_vapourwave_palette 64 in
  let fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let screen = Screen.create width height scale palette in

  Animation.start_recording recording_state 10;
  Animation.record_frame recording_state screen fb;
  assert_raises (Failure "Already recording animation") (fun () ->
      Animation.start_recording recording_state 10);
  Animation.stop_recording recording_state

let test_palette_too_big _ =
  let recording_state = ref None in
  let palette = Palette.generate_mono_palette 300 in
  let fb = Framebuffer.init (width, height) (fun _ _ -> 42) in
  let screen = Screen.create width height scale palette in

  Animation.start_recording recording_state 10;
  assert_raises (Failure "GIF only supports up to 256 colors") (fun () ->
      Animation.record_frame recording_state screen fb);
  assert_raises (Giflib.GIF.Error "from_images: empty image list") (fun () ->
      Animation.stop_recording recording_state)

let suite =
  "animation_tests"
  >::: [
         "Test basic recording" >:: test_basic_recording;
         "Test invalid frame count" >:: test_invalid_frame_count;
         "Test double recording" >:: test_double_recording;
         "Test palette too big" >:: test_palette_too_big;
       ]

let () = run_test_tt_main suite
