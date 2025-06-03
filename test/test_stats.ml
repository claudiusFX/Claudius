open Claudius
open OUnit2

let test_update_stats_long _ =
  let initial = Stats.create () in
  assert_equal ~msg:"FPS" 0 (Stats.fps initial);
  let updated = Stats.update ~now:1.1 ~tick:29 initial in
  assert_equal ~msg:"FPS" 29 (Stats.fps updated)

let test_update_stats_short _ =
  let initial = Stats.create () in
  assert_equal ~msg:"FPS" 0 (Stats.fps initial);
  let updated = Stats.update ~now:0.5 ~tick:29 initial in
  assert_equal ~msg:"FPS" 0 (Stats.fps updated)

let suite =
  "StatsTests"
  >::: [
         "Test stats long update" >:: test_update_stats_long;
         "Test stats short update" >:: test_update_stats_short;
       ]

let () = run_test_tt_main suite
