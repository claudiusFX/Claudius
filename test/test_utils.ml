open Claudius
open OUnit2

let test_points_to_lines_empty _ =
	let res = Utils.points_to_lines [] in
	assert_equal [] res

let test_points_to_lines_nearly_empty _ =
	let res = Utils.points_to_lines [1] in
	assert_equal [] res

let test_points_to_lines_simple_list _ =
	let res = Utils.points_to_lines [1; 2 ; 3] in
	assert_equal [(2, 3); (1, 2); (3, 1) ] res

let suite =
  "Utils tests" >::: [
	"Test points to line on empty list" >:: test_points_to_lines_empty ;
	"Test points to line on nearly empty list" >:: test_points_to_lines_nearly_empty ;
	"Test points to line on smallest list" >:: test_points_to_lines_simple_list ;
  ]

let () =
  run_test_tt_main suite
