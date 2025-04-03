
(* FPS Calculation *)
let last_time = ref 0.0
let frame_count = ref 0
let fps_counter = ref 0

(* Stats toggle key *)
let stats_toggle_key = Key.F

let calculate_fps () =
  let now = Unix.gettimeofday () in
  let elapsed = now -. !last_time in
  frame_count := !frame_count + 1;
  if elapsed >= 1.0 then (
    fps_counter := !frame_count;
    frame_count := 0;
    last_time := now
  )

(* Font rendering *)
let draw_char x y c brightness create_pixel acc =
  match c with
  | 'F' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
       create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'P' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'S' ->
     let pixels = [
      (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'R' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+2, y+3);
      (x, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'E' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'D' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'O' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'T' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | ':' ->
     let pixels = [
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'x' ->
     let pixels = [
      (x, y); (x+3, y);
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3);
      (x, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | ' ' -> acc 
  | '0' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels

  (* Add definitions for digits 1-9 *)
  | '1' ->
     let pixels = [
      (x+1, y);
      (x, y+1); (x+1, y+1);
      (x+1, y+2);
      (x+1, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '2' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+2, y+2); (x+3, y+2);
      (x+1, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '3' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x+3, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '4' ->
     let pixels = [
      (x, y); (x+3, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2); (x+3, y+2);
      (x+3, y+3);
      (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '5' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '6' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '7' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x+3, y+1);
      (x+2, y+2);
      (x+1, y+3);
      (x+1, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '8' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '9' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+1, y+2); (x+2, y+2); (x+3, y+2);
      (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | _ -> acc 

let draw_string x y text brightness create_pixel acc =
  let chars = List.of_seq (String.to_seq text) in
  let rec aux x y chars acc =
    match chars with
    | [] -> acc
    | c :: cs -> 
      let new_acc = draw_char (x/2) (y/2) c brightness create_pixel acc in
      aux (x + 10) y cs new_acc 
  in
  aux x y chars acc

(* Render stats function *)
let render_stats create_pixel entity_count width height =
  calculate_fps ();
  
  let fps_text = Printf.sprintf "FPS: %d" !fps_counter in
  let entity_text = Printf.sprintf "DOTS: %d" entity_count in
  let res_text = Printf.sprintf "RES: %dx%d" width height in
  
  let base_x = 2 in
  let base_y = 2 in
  let brightness = 15 in
  
  let fps_primitives = draw_string (base_x*2) (base_y*2) fps_text brightness create_pixel [] in
  let entity_primitives = draw_string (base_x*2) ((base_y + 14)*2) entity_text brightness create_pixel fps_primitives in
  draw_string (base_x*2) ((base_y + 28)*2) res_text brightness create_pixel entity_primitives