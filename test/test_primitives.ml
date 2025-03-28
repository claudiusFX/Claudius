open Claudius
open OUnit2

(* Line *)

let test_draw_line_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_line 3 3 7 7 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 5 5 fb)

let test_draw_line_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_line (-3) (-3) 7 7 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 5 5 fb)

let test_draw_line_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let line = Primitives.Line ({x = 3; y = 3}, {x = 7 ; y = 7}, 1) in
  Framebuffer.render fb [line];
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 5 5 fb)

(* Pixel *)

let test_draw_pixel_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.Pixel ({x = 5; y = 5}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 5 5 fb)

(* Circle *)

let test_draw_circle_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_circle 5 5 2.0 1 fb;
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 7 fb)

let test_draw_circle_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_circle (-1) (-1) 3.0 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 1 1 fb)

let test_draw_circle_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.Circle ({x = 5; y = 5}, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 7 fb)

(* Filled circle *)

let test_draw_filled_circle_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_circle 5 5 2.0 1 fb;
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 7 fb)

let test_draw_filled_circle_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_circle (-1) (-1) 3.0 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 1 1 fb)

let test_draw_filled_circle_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.FilledCircle ({x = 5; y = 5}, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 7 fb)

(* Rect *)

let test_draw_rect_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_rect 3 3 3 3 1 fb;
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 6 6 fb)

let test_draw_rect_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_rect (-1) (-1) 3 3 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_rect_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Rect ({x = 3; y = 3}, {x = 6; y = 6}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 6 6 fb)

(* Filled rect *)

let test_draw_filled_rect_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_rect 3 3 3 3 1 fb;
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 6 6 fb)

let test_draw_filled_rect_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_rect (-1) (-1) 3 3 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_filled_rect_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledRect ({x = 3; y = 3}, {x = 6; y = 6}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 6 6 fb)

(* Triangle *)

let test_draw_triangle_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_triangle 3 3 5 8 8 3 1 fb;
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

let test_draw_triangle_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_triangle (-1) (-1) 3 3 (-5) 3 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_triangle_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Triangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

(* Filled triangle *)

let test_draw_filled_triangle_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_triangle 3 3 5 8 8 3 1 fb;
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

let test_draw_filled_triangle_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_triangle (-1) (-1) 3 3 (-5) 3 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_filled_triangle_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledTriangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

(* Polygon *)

let test_draw_polygon_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_polygon [(3, 3) ; (5, 8) ; (8, 3)] 1 fb;
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

let test_draw_polygon_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_polygon [(-1, -1) ; (3, 3) ; (-5, 3)] 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_polygon_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Polygon ([{x = 3; y = 3} ; {x = 5; y = 8} ; {x = 8; y = 3}], 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

(* Filled polygon *)

let test_draw_filled_polygon_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_polygon [(3, 3) ; (5, 8) ; (8, 3)] 1 fb;
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

let test_draw_filled_polygon_direct_off_framebuffer _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_polygon [(-1, -1) ; (3, 3) ; (-5, 3)] 1 fb;
  assert_equal ~msg:"after" (Some 1) (Framebuffer.pixel_read 2 2 fb)

let test_draw_filled_polygon_with_primitive _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledPolygon ([{x = 3; y = 3} ; {x = 5; y = 8} ; {x = 8; y = 3}], 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge" (Some 1) (Framebuffer.pixel_read 5 3 fb)

  let test_dirty_bit_draw_line _ =
    let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
    Framebuffer.clear_dirty fb;
    assert_equal ~msg:"dirty bit should be false after clear" false (Framebuffer.is_dirty fb);
    Framebuffer.draw_line 3 3 7 7 1 fb;
    assert_equal ~msg:"dirty bit should be set after draw_line" true (Framebuffer.is_dirty fb)
  
  let test_dirty_bit_render_primitive _ =
    let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
    Framebuffer.clear_dirty fb;
    assert_equal ~msg:"dirty bit should be false after clear" false (Framebuffer.is_dirty fb);
    let prim = Primitives.Pixel ({x = 5; y = 5}, 1) in
    Framebuffer.render fb [prim];
    assert_equal ~msg:"dirty bit should be set after render" true (Framebuffer.is_dirty fb)

let suite =
  "Primitives tests" >::: [
    "Test draw line direct" >:: test_draw_line_direct ;
    "Test draw line direct off framebuffer" >:: test_draw_line_direct_off_framebuffer ;
    "Test draw line with primative" >:: test_draw_line_with_primitive ;
    "Test draw pixel with primative" >:: test_draw_pixel_with_primitive ;
    "Test draw circle direct" >:: test_draw_circle_direct ;
    "Test draw circle direct off framebuffer" >:: test_draw_circle_direct_off_framebuffer ;
    "Test draw circle with primative" >:: test_draw_circle_with_primitive ;
    "Test filled circle direct" >:: test_draw_filled_circle_direct ;
    "Test filled circle direct off framebuffer" >:: test_draw_filled_circle_direct_off_framebuffer ;
    "Test filled circle with primative" >:: test_draw_filled_circle_with_primitive ;
    "Test draw rect direct" >:: test_draw_rect_direct ;
    "Test draw rect direct off framebuffer" >:: test_draw_rect_direct_off_framebuffer ;
    "Test draw rect with primative" >:: test_draw_rect_with_primitive ;
    "Test filled rect direct" >:: test_draw_filled_rect_direct ;
    "Test filled rect direct off framebuffer" >:: test_draw_filled_rect_direct_off_framebuffer ;
    "Test filled rect with primative" >:: test_draw_filled_rect_with_primitive ;
    "Test draw triangle direct" >:: test_draw_triangle_direct ;
    "Test draw triangle direct off framebuffer" >:: test_draw_triangle_direct_off_framebuffer ;
    "Test draw triangle with primative" >:: test_draw_triangle_with_primitive ;
    "Test filled triangle direct" >:: test_draw_filled_triangle_direct ;
    "Test filled triangle direct off framebuffer" >:: test_draw_filled_triangle_direct_off_framebuffer ;
    "Test filled triangle with primative" >:: test_draw_filled_triangle_with_primitive ;
    "Test draw polygon direct" >:: test_draw_polygon_direct ;
    "Test draw polygon direct off framebuffer" >:: test_draw_polygon_direct_off_framebuffer ;
    "Test draw polygon with primative" >:: test_draw_polygon_with_primitive ;
    "Test filled polygon direct" >:: test_draw_filled_polygon_direct ;
    "Test filled polygon direct off framebuffer" >:: test_draw_filled_polygon_direct_off_framebuffer ;
    "Test filled polygon with primative" >:: test_draw_filled_polygon_with_primitive ;
    "Test dirty bit after draw_line" >:: test_dirty_bit_draw_line;
    "Test dirty bit after render primitive" >:: test_dirty_bit_render_primitive;
  ]

let () =
  run_test_tt_main suite