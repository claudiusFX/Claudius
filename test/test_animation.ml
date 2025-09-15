open OUnit2
open Claudius

let width, height = (100, 100)
let scale = 2

let test_basic_recording _ =
  let palette = Palette.generate_vapourwave_palette 64 in
  let fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let screen = Screen.create width height scale palette in
  let initial_state = Animation.start_recording 10 in
  match initial_state with
  | Result.Error _msg -> assert false
  | Result.Ok state ->
      let mut_state = ref (Some state) in

      for _ = 1 to 10 do
        match !mut_state with
        | Some st -> mut_state := Animation.record_frame st screen fb
        | None -> ()
      done;

      assert_equal None !mut_state

let test_invalid_frame_count _ =
  let res = Animation.start_recording 0 in
  assert_equal (Result.Error "Number of frames must be positive") res

let test_too_many_frames _ =
  let msg =
    Printf.sprintf "Maximum %d frames allowed" Animation.max_frames_default
  in
  let res = Animation.start_recording (Animation.max_frames_default + 1) in
  assert_equal (Result.Error msg) res

let test_double_recording _ =
  let palette = Palette.generate_vapourwave_palette 64 in
  let fb = Framebuffer.init (width, height) (fun x y -> x * y mod 64) in
  let screen = Screen.create width height scale palette in
  let initial_state = Animation.start_recording 10 in
  match initial_state with
  | Result.Error _msg -> assert false
  | Result.Ok state ->
      let mut_state = ref (Some state) in

      mut_state := Animation.record_frame (Option.get !mut_state) screen fb;

      let already_recording = Option.is_some !mut_state in
      assert_bool "Should detect already recording state" already_recording;

      ignore (Animation.stop_recording (Option.get !mut_state));
      mut_state := None

let test_palette_too_big _ =
  let palette = Palette.generate_mono_palette 300 in
  let fb = Framebuffer.init (width, height) (fun _ _ -> 42) in
  let screen = Screen.create width height scale palette in

  let initial_state = Animation.start_recording 10 in
  match initial_state with
  | Result.Error _msg -> assert false
  | Result.Ok state ->
      assert_raises (Failure "GIF only supports up to 256 colors") (fun () ->
          ignore (Animation.record_frame state screen fb));
      assert_raises (Giflib.GIF.Error "from_images: empty image list")
        (fun () -> ignore (Animation.stop_recording state))

let suite =
  "animation_tests"
  >::: [
         "Test basic recording" >:: test_basic_recording;
         "Test invalid frame count" >:: test_invalid_frame_count;
         "Test too many frames" >:: test_too_many_frames;
         "Test double recording" >:: test_double_recording;
         "Test palette too big" >:: test_palette_too_big;
       ]

let () = run_test_tt_main suite
