open Claudius
open OUnit2

(* Helper: prepare a framebuffer by clearing its dirty bit and asserting it's clear *)
let prepare_fb dims init_fun =
  let fb = Framebuffer.init dims init_fun in
  Framebuffer.clear_dirty fb;
  assert_equal ~msg:"dirty bit should be false after clear" false (Framebuffer.is_dirty fb);
  fb

(* Line *)

let test_draw_line_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (line direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_line 3 3 7 7 1 fb;
  assert_equal ~msg:"after (line direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"dirty bit should be set after draw_line" true (Framebuffer.is_dirty fb)


let test_draw_line_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (line off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_line (-3) (-3) 7 7 1 fb;
  assert_equal ~msg:"after (line off)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"dirty bit should be set after draw_line (off)" true (Framebuffer.is_dirty fb)

let test_draw_line_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (line prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let line = Primitives.Line ({x = 3; y = 3}, {x = 7 ; y = 7}, 1) in
  Framebuffer.render fb [line];
  assert_equal ~msg:"after (line prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"dirty bit should be set after render (line)" true (Framebuffer.is_dirty fb)

(* Pixel *)
let test_draw_pixel_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (pixel prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.Pixel ({x = 5; y = 5}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after (pixel prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"dirty bit should be set after render (pixel)" true (Framebuffer.is_dirty fb)

(* Circle *)
let test_draw_circle_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (circle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_circle 5 5 2.0 1 fb;
  assert_equal ~msg:"after center (circle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (circle direct)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after draw_circle" true (Framebuffer.is_dirty fb)

let test_draw_circle_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (circle off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_circle (-1) (-1) 3.0 1 fb;
  assert_equal ~msg:"after (circle off)" (Some 1) (Framebuffer.pixel_read 1 1 fb);
  assert_equal ~msg:"dirty bit should be set after draw_circle (off)" true (Framebuffer.is_dirty fb)

let test_draw_circle_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (circle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.Circle ({x = 5; y = 5}, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (circle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (circle prim)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after render (circle)" true (Framebuffer.is_dirty fb)

(* Filled circle *)

let test_draw_filled_circle_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled circle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_circle 5 5 2.0 1 fb;
  assert_equal ~msg:"after center (filled circle direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled circle direct)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after filled_circle" true (Framebuffer.is_dirty fb)

let test_draw_filled_circle_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled circle off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_circle (-1) (-1) 3.0 1 fb;
  assert_equal ~msg:"after (filled circle off)" (Some 1) (Framebuffer.pixel_read 1 1 fb);
  assert_equal ~msg:"dirty bit should be set after filled_circle (off)" true (Framebuffer.is_dirty fb)

let test_draw_filled_circle_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled circle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.FilledCircle ({x = 5; y = 5}, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (filled circle prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled circle prim)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after render (filled circle)" true (Framebuffer.is_dirty fb)

(* Ellipse *)

let test_draw_ellipse_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (ellipse direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_ellipse 5 5 3.0 2.0 1 fb;
  assert_equal ~msg:"after center (ellipse direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (ellipse direct)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after draw_ellipse" true (Framebuffer.is_dirty fb)


let test_draw_ellipse_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (ellipse off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_ellipse (-5) (-5) 4.0 3.0 1 fb;
  assert_equal ~msg:"after (ellipse off)" (Some 0) (Framebuffer.pixel_read 0 0 fb);
  assert_equal ~msg:"dirty bit should be set after draw_ellipse (off)" true (Framebuffer.is_dirty fb)

let test_draw_ellipse_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (ellipse prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.Ellipse ({x = 5; y = 5}, 3.0, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (ellipse prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (ellipse prim)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after render (ellipse)" true (Framebuffer.is_dirty fb)

(* Filled ellipse *)

let test_draw_filled_ellipse_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled ellipse direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_ellipse 5 5 3.0 2.0 1 fb;
  assert_equal ~msg:"after center (filled ellipse direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled ellipse direct)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after filled_ellipse" true (Framebuffer.is_dirty fb)

let test_draw_filled_ellipse_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled ellipse off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_ellipse 0 0 4.0 3.0 1 fb;
  assert_equal ~msg:"after (filled ellipse off)" (Some 1) (Framebuffer.pixel_read 1 1 fb);
  assert_equal ~msg:"dirty bit should be set after filled_ellipse (off)" true (Framebuffer.is_dirty fb)

let test_draw_filled_ellipse_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled ellipse prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  let prim = Primitives.FilledEllipse ({x = 5; y = 5}, 3.0, 2.0, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (filled ellipse prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled ellipse prim)" (Some 1) (Framebuffer.pixel_read 5 7 fb);
  assert_equal ~msg:"dirty bit should be set after render (filled ellipse)" true (Framebuffer.is_dirty fb)



(* Rect *)

let test_draw_rect_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (rect direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (rect direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_rect 3 3 3 3 1 fb;
  assert_equal ~msg:"after center (rect direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (rect direct)" (Some 1) (Framebuffer.pixel_read 6 6 fb);
  assert_equal ~msg:"dirty bit should be set after draw_rect" true (Framebuffer.is_dirty fb)

let test_draw_rect_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (rect off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_rect (-1) (-1) 3 3 1 fb;
  assert_equal ~msg:"after (rect off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after draw_rect (off)" true (Framebuffer.is_dirty fb)

let test_draw_rect_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (rect prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (rect prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Rect ({x = 3; y = 3}, {x = 6; y = 6}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (rect prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (rect prim)" (Some 1) (Framebuffer.pixel_read 6 6 fb);
  assert_equal ~msg:"dirty bit should be set after render (rect)" true (Framebuffer.is_dirty fb)

(* Filled rect *)

let test_draw_filled_rect_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled rect direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled rect direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_rect 3 3 3 3 1 fb;
  assert_equal ~msg:"after center (filled rect direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled rect direct)" (Some 1) (Framebuffer.pixel_read 6 6 fb);
  assert_equal ~msg:"dirty bit should be set after filled_rect" true (Framebuffer.is_dirty fb)

let test_draw_filled_rect_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled rect off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_rect (-1) (-1) 3 3 1 fb;
  assert_equal ~msg:"after (filled rect off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after filled_rect (off)" true (Framebuffer.is_dirty fb)

let test_draw_filled_rect_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled rect prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled rect prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledRect ({x = 3; y = 3}, {x = 6; y = 6}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after center (filled rect prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after edge (filled rect prim)" (Some 1) (Framebuffer.pixel_read 6 6 fb);
  assert_equal ~msg:"dirty bit should be set after render (filled rect)" true (Framebuffer.is_dirty fb)

(* Triangle *)

let test_draw_triangle_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (triangle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (triangle direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_triangle 3 3 5 8 8 3 1 fb;
  assert_equal ~msg:"after (triangle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (triangle direct)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after draw_triangle" true (Framebuffer.is_dirty fb)

let test_draw_triangle_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (triangle off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_triangle (-1) (-1) 3 3 (-5) 3 1 fb;
  assert_equal ~msg:"after (triangle off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after draw_triangle (off)" true (Framebuffer.is_dirty fb)

let test_draw_triangle_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (triangle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (triangle prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Triangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after (triangle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (triangle prim)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after render (triangle)" true (Framebuffer.is_dirty fb)

(* Filled triangle *)

let test_draw_filled_triangle_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled triangle direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled triangle direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_triangle 3 3 5 8 8 3 1 fb;
  assert_equal ~msg:"after (filled triangle direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (filled triangle direct)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after filled_triangle" true (Framebuffer.is_dirty fb)

let test_draw_filled_triangle_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled triangle off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_triangle (-1) (-1) 3 3 (-5) 3 1 fb;
  assert_equal ~msg:"after (filled triangle off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after filled_triangle (off)" true (Framebuffer.is_dirty fb)

let test_draw_filled_triangle_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled triangle prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled triangle prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledTriangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after (filled triangle prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (filled triangle prim)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after render (filled triangle)" true (Framebuffer.is_dirty fb)

(* Polygon *)
let test_draw_polygon_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (polygon direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (polygon direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.draw_polygon [(3, 3) ; (5, 8) ; (8, 3)] 1 fb;
  assert_equal ~msg:"after (polygon direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (polygon direct)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after draw_polygon" true (Framebuffer.is_dirty fb)

let test_draw_polygon_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (polygon off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_polygon [(-1, -1) ; (3, 3) ; (-5, 3)] 1 fb;
  assert_equal ~msg:"after (polygon off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after draw_polygon (off)" true (Framebuffer.is_dirty fb)

let test_draw_polygon_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (polygon prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (polygon prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.Polygon ([{x = 3; y = 3} ; {x = 5; y = 8} ; {x = 8; y = 3}], 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after (polygon prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (polygon prim)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after render (polygon)" true (Framebuffer.is_dirty fb)

(* Filled polygon *)

let test_draw_filled_polygon_direct _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled polygon direct)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled polygon direct)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  Framebuffer.filled_polygon [(3, 3); (5, 8); (8, 3)] 1 fb;
  assert_equal ~msg:"after (filled polygon direct)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (filled polygon direct)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after filled_polygon" true (Framebuffer.is_dirty fb)

let test_draw_filled_polygon_direct_off_framebuffer _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled polygon off)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.filled_polygon [(-1, -1) ; (3, 3) ; (-5, 3)] 1 fb;
  assert_equal ~msg:"after (filled polygon off)" (Some 1) (Framebuffer.pixel_read 2 2 fb);
  assert_equal ~msg:"dirty bit should be set after filled_polygon (off)" true (Framebuffer.is_dirty fb)

let test_draw_filled_polygon_with_primitive _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before (filled polygon prim)" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"before (filled polygon prim)" (Some 0) (Framebuffer.pixel_read 7 7 fb);
  let prim = Primitives.FilledPolygon ([{x = 3; y = 3} ; {x = 5; y = 8} ; {x = 8; y = 3}], 1) in
  Framebuffer.render fb [prim];
  assert_equal ~msg:"after (filled polygon prim)" (Some 1) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"after (filled polygon prim)" (Some 1) (Framebuffer.pixel_read 5 3 fb);
  assert_equal ~msg:"dirty bit should be set after render (filled polygon)" true (Framebuffer.is_dirty fb)

(* ----- Dirty Bit Test For All Primitives ----- *)
let test_dirty_bit_render_primitive _ =
  let primitives = [
    Primitives.Pixel ({x = 5; y = 5}, 1);
    Primitives.Line ({x = 2; y = 2}, {x = 5; y = 5}, 1);
    Primitives.Circle ({x = 5; y = 5}, 4.0, 1);
    Primitives.FilledCircle ({x = 5; y = 5}, 4.0, 1);
    Primitives.Ellipse ({x = 5; y = 5}, 4.0, 3.0, 1);
    Primitives.FilledEllipse ({x = 5; y = 5}, 4.0, 3.0, 1);
    Primitives.Rect ({x = 3; y = 3}, {x = 6; y = 6}, 1);
    Primitives.FilledRect ({x = 3; y = 3}, {x = 6; y = 6}, 1);
    Primitives.Triangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1);
    Primitives.FilledTriangle ({x = 3; y = 3}, {x = 5; y = 8}, {x = 8; y = 3}, 1);
    Primitives.Polygon ([{x = 3; y = 3}; {x = 5; y = 8}; {x = 8; y = 3}], 1);
    Primitives.FilledPolygon ([{x = 3; y = 3}; {x = 5; y = 8}; {x = 8; y = 3}], 1)
  ] in
  List.iter (fun prim ->
    let fb = prepare_fb (10, 10) (fun _ _ -> 0) in
    Framebuffer.render fb [prim];
    assert_equal ~msg:"dirty bit should be set after render (all prim)" true (Framebuffer.is_dirty fb)
  ) primitives

(* ----- Map Functions Tests ----- *)
let test_map _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 1) in
  let fb2 = Framebuffer.map (fun x -> x + 1) fb in
  assert_equal ~msg:"map should increase pixel value" (Some 2) (Framebuffer.pixel_read 5 5 fb2);
  assert_equal ~msg:"dirty bit should be set in new framebuffer from map" true (Framebuffer.is_dirty fb2)

let test_mapi _ =
  let fb = prepare_fb (10, 10) (fun x y -> x + y) in
  let fb2 = Framebuffer.mapi (fun x y _ -> x * y) fb in
  assert_equal ~msg:"mapi should compute product" (Some 12) (Framebuffer.pixel_read 3 4 fb2);
  assert_equal ~msg:"dirty bit should be set in new framebuffer from mapi" true (Framebuffer.is_dirty fb2)

let test_map_inplace _ =
  let fb = prepare_fb (10, 10) (fun _ _ -> 1) in
  Framebuffer.map_inplace (fun x -> x * 2) fb;
  assert_equal ~msg:"map_inplace should double pixel" (Some 2) (Framebuffer.pixel_read 5 5 fb);
  assert_equal ~msg:"dirty bit should be set after map_inplace" true (Framebuffer.is_dirty fb)

let test_mapi_inplace _ =
  let fb = prepare_fb (10, 10) (fun x y -> x + y) in
  Framebuffer.mapi_inplace (fun x y _ -> x * y) fb;
  assert_equal ~msg:"mapi_inplace should compute product" (Some 12) (Framebuffer.pixel_read 3 4 fb);
  assert_equal ~msg:"dirty bit should be set after mapi_inplace" true (Framebuffer.is_dirty fb)

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
    "Test draw ellipse direct" >:: test_draw_ellipse_direct ;
    "Test draw ellipse direct off framebuffer" >:: test_draw_ellipse_direct_off_framebuffer ;
    "Test draw ellipse with primitive" >:: test_draw_ellipse_with_primitive ;
    "Test filled ellipse direct" >:: test_draw_filled_ellipse_direct ;
    "Test filled ellipse direct off framebuffer" >:: test_draw_filled_ellipse_direct_off_framebuffer ;
    "Test filled ellipse with primitive" >:: test_draw_filled_ellipse_with_primitive ;
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
    "Test dirty bit render primitive (all prim)" >:: test_dirty_bit_render_primitive;
    "Test map" >:: test_map ;
    "Test mapi" >:: test_mapi ;
    "Test map_inplace" >:: test_map_inplace ;
    "Test mapi_inplace" >:: test_mapi_inplace ;
  ]

let () =
  run_test_tt_main suite