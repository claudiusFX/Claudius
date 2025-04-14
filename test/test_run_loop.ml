open OUnit2
open Claudius
open Tsdl

module TestRunLoop = MakeRunLoop(MockSDL)

let dummy_tick (t : int) (_screen : Screen.t) (prev_fb : Framebuffer.t) (_input : input_state) : Framebuffer.t =
  prev_fb

let test_always_fail _ =
   let test_always_fail _ =
  assert_equal ~msg:"This should fail" 1 2assert_equal ~msg:"This should fail" 1 2

(* Test that the run loop with a dummy tick (no update) returns a framebuffer equal
   to the initial one.*)

let test_run_loop_no_update _ =
  let screen = Screen.create 320 240 1 (Palette.generate_mono_palette 16) in
  let initial_fb = Framebuffer.init (320, 240) (fun _x _y -> 0) in
  let result_fb = TestRunLoop.run "TestRun" None dummy_tick screen in
  assert_equal (Framebuffer.to_array initial_fb) (Framebuffer.to_array result_fb)

let suite =
  "Run Loop Unit Tests" >::: [
    "Test fail" >:: test_always_fail;
    "Test run loop with dummy tick (no update)" >:: test_run_loop_no_update;
  ]

let () =
  run_test_tt_main suite