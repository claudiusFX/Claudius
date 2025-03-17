
let points_to_lines points =
	(* The caller to this has already sorted the list, and so is sensitive to return order *)
	let rec last list =
		match list with
		| [] -> failwith "shouldn't get here"
		| [x] -> x
		| _ :: xs -> last xs
	in
	let res = match points with
	| [] | [_] -> []
	| p1 :: p2 :: ptl -> (
	  let hd = p1 in
	  let tl = last points in
	  let rec point_list_to_lines p1 p2 pl acc =
		match pl with
		| [] -> (p1, p2) :: acc
		| np :: tl -> point_list_to_lines p2 np tl ((p1, p2) :: acc)
	  in
	  point_list_to_lines p1 p2 ptl [(tl, hd)]
	) in
	res
