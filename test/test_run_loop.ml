(* test_run_loop.ml *)

open OUnit2
open Claudius  (* This should provide Screen, Framebuffer, Palette, etc. *)
open Tsdl

(* Instantiate the run loop with our mock SDL implementation *)
module TestRunLoop = MakeRunLoop(MockSDL)

(* A dummy tick function that performs no updates:
   It simply returns the framebuffer unchanged.
*)
let dummy_tick (t : int) (_screen : Screen.t) (prev_fb : Framebuffer.t) (_input : input_state) : Framebuffer.t =
  prev_fb

(* Test that using the dummy tick function leaves the framebuffer unchanged *)
let test_run_loop_no_update _ =
  (* Create a test screen with a simple monochrome palette *)
  let screen = Screen.create 320 240 1 (Palette.generate_mono_palette 16) in
  (* Create an initial framebuffer filled with zeroes *)
  let initial_fb = Framebuffer.init (320, 240) (fun _x _y -> 0) in
  (* Run the test run loop once; dummy_tick should return the initial framebuffer *)
  let result_fb = TestRunLoop.run "TestRun" None dummy_tick screen in
  (* Compare the underlying pixel arrays of the initial and resulting framebuffers *)
  assert_equal (Framebuffer.to_array initial_fb) (Framebuffer.to_array result_fb)

(* Group the tests into a suite *)
let suite =
  "Run Loop Unit Tests" >::: [
    "Test run loop with dummy tick (no update)" >:: test_run_loop_no_update;
  ]

(* Execute the test suite *)
let () =
  run_test_tt_main suite
