open OUnit2
open Claudius
open Tsdl

let setup scale = (Sdl.Event.create (), Mouse.create scale)

let test_invalid_scale _ =
  assert_raises (Invalid_argument "Invalid scale") (fun () -> Mouse.create 0);
  assert_raises (Invalid_argument "Invalid scale") (fun () -> Mouse.create (-1))

let test_of_sdl_button _ =
  assert_equal Mouse.Left (Mousesdl.of_sdl_button 1);
  assert_equal Mouse.Middle (Mousesdl.of_sdl_button 2);
  assert_equal Mouse.Right (Mousesdl.of_sdl_button 3);
  assert_equal Mouse.Left (Mousesdl.of_sdl_button 0)

let test_to_sdl_button _ =
  assert_equal 1 (Mousesdl.to_sdl_button Mouse.Left);
  assert_equal 2 (Mousesdl.to_sdl_button Mouse.Middle);
  assert_equal 3 (Mousesdl.to_sdl_button Mouse.Right)

let test_handle_mouse_button_event _ =
  let event, mouse = setup 1 in
  (* Test button down *)
  Sdl.Event.set event Sdl.Event.mouse_button_button 1;
  Sdl.Event.set event Sdl.Event.mouse_button_x 100;
  Sdl.Event.set event Sdl.Event.mouse_button_y 200;
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
  let updated_mouse, _ = Mousesdl.handle_mouse_button_event event mouse in
  assert_equal (100, 200) (Mouse.get_position updated_mouse);
  assert_equal true (Mouse.is_button_pressed updated_mouse Mouse.Left);
  (* Test button up *)
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.released;
  let updated_mouse2, _ =
    Mousesdl.handle_mouse_button_event event updated_mouse
  in
  assert_equal false (Mouse.is_button_pressed updated_mouse2 Mouse.Left)

let test_handle_mouse_motion_event _ =
  let event, mouse = setup 2 in

  Sdl.Event.set event Sdl.Event.mouse_motion_x 150;
  Sdl.Event.set event Sdl.Event.mouse_motion_y 250;
  let updated_mouse, _ = Mousesdl.handle_mouse_motion_event event mouse in
  (* For scale 2, (150,250) becomes (75,125) *)
  assert_equal (75, 125) (Mouse.get_position updated_mouse)

let test_handle_mouse_wheel_event _ =
  let event, mouse = setup 1 in

  Sdl.Event.set event Sdl.Event.mouse_wheel_y 1;
  (* Scroll up *)
  let _updated_mouse, events = Mousesdl.handle_mouse_wheel_event event mouse in
  match events with
  | [ Event.MouseWheel 1 ] -> ()
  | _ -> failwith "Expected Wheel event with positive value"

(* Split test_handle_event into three separate tests *)
let test_handle_event_button _ =
  let event, mouse = setup 1 in

  Sdl.Event.set event Sdl.Event.mouse_button_button 1;
  Sdl.Event.set event Sdl.Event.mouse_button_x 100;
  Sdl.Event.set event Sdl.Event.mouse_button_y 200;
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_button_down;
  let updated_mouse, _ = Mousesdl.handle_event event mouse in
  assert_equal (100, 200) (Mouse.get_position updated_mouse);
  assert_equal true (Mouse.is_button_pressed updated_mouse Mouse.Left)

let test_handle_event_motion _ =
  let event, mouse = setup 1 in

  Sdl.Event.set event Sdl.Event.mouse_motion_x 150;
  Sdl.Event.set event Sdl.Event.mouse_motion_y 250;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_motion;
  let updated_mouse, _ = Mousesdl.handle_event event mouse in
  assert_equal (150, 250) (Mouse.get_position updated_mouse)

let test_handle_event_wheel _ =
  let event, mouse = setup 1 in

  Sdl.Event.set event Sdl.Event.mouse_wheel_y 1;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_wheel;
  let _updated_mouse, events = Mousesdl.handle_event event mouse in
  match events with
  | [ Event.MouseWheel 1 ] -> ()
  | _ -> failwith "Expected Wheel event with positive value"

let test_drag_for_all_buttons _ =
  let event, mouse = setup 1 in
  let buttons = [ Mouse.Left; Mouse.Middle; Mouse.Right ] in

  List.iter
    (fun button ->
      (* Press button *)
      Sdl.Event.set event Sdl.Event.mouse_button_button
        (Mousesdl.to_sdl_button button);
      Sdl.Event.set event Sdl.Event.mouse_button_x 10;
      Sdl.Event.set event Sdl.Event.mouse_button_y 20;
      Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
      let mouse_after_press, _ =
        Mousesdl.handle_mouse_button_event event mouse
      in
      (* Drag motion *)
      Sdl.Event.set event Sdl.Event.mouse_motion_x 30;
      Sdl.Event.set event Sdl.Event.mouse_motion_y 40;
      Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_motion;
      let _mouse_after_drag, events =
        Mousesdl.handle_event event mouse_after_press
      in
      (* Assert Drag exists *)
      assert_bool
        (Printf.sprintf "Expected Drag event for %s button"
           (match button with
           | Mouse.Left -> "Left"
           | Mouse.Middle -> "Middle"
           | Mouse.Right -> "Right"))
        (List.exists
           (function
             | Event.MouseDrag (b, (30, 40)) when b = button -> true
             | _ -> false)
           events);

      (* Assert Motion is not present *)
      assert_bool "Should not contain Motion event during drag"
        (not
           (List.exists
              (function Event.MouseMotion _ -> true | _ -> false)
              events)))
    buttons

let test_motion_after_drag_release _ =
  let buttons = [ Mouse.Left; Mouse.Middle; Mouse.Right ] in
  List.iter
    (fun button ->
      let event, mouse = setup 1 in
      (* Press current button *)
      Sdl.Event.set event Sdl.Event.mouse_button_button
        (Mousesdl.to_sdl_button button);
      Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
      let mouse_after_press, _ =
        Mousesdl.handle_mouse_button_event event mouse
      in
      (* Simulate a motion event to produce a Drag event *)
      Sdl.Event.set event Sdl.Event.mouse_motion_x 30;
      Sdl.Event.set event Sdl.Event.mouse_motion_y 40;
      Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_motion;
      let mouse_after_drag, _ = Mousesdl.handle_event event mouse_after_press in
      (* Release the button *)
      Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.released;
      let mouse_after_release, _ =
        Mousesdl.handle_mouse_button_event event mouse_after_drag
      in
      (* Simulate another motion event → should be Motion, not Drag *)
      Sdl.Event.set event Sdl.Event.mouse_motion_x 50;
      Sdl.Event.set event Sdl.Event.mouse_motion_y 60;
      let _mouse_after_motion, events =
        Mousesdl.handle_event event mouse_after_release
      in
      assert_bool
        (Printf.sprintf "Expected Motion event for %s button after release"
           (match button with
           | Mouse.Left -> "Left"
           | Mouse.Middle -> "Middle"
           | Mouse.Right -> "Right"))
        (List.exists
           (function Event.MouseMotion (50, 60) -> true | _ -> false)
           events);

      assert_bool
        (Printf.sprintf
           "Should not contain Drag event for %s button after release"
           (match button with
           | Mouse.Left -> "Left"
           | Mouse.Middle -> "Middle"
           | Mouse.Right -> "Right"))
        (not
           (List.exists
              (function Event.MouseDrag _ -> true | _ -> false)
              events)))
    buttons

let test_multiple_drag_events _ =
  let event = Sdl.Event.create () in
  let event2 = Sdl.Event.create () in
  let mouse = Mouse.create 1 in

  (* Press Left button *)
  Sdl.Event.set event Sdl.Event.mouse_button_button
    (Mousesdl.to_sdl_button Mouse.Left);
  Sdl.Event.set event Sdl.Event.mouse_button_x 10;
  Sdl.Event.set event Sdl.Event.mouse_button_y 20;
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
  let mouse_after_left, _ = Mousesdl.handle_mouse_button_event event mouse in
  (* Press Middle button using a second event *)
  Sdl.Event.set event2 Sdl.Event.mouse_button_button
    (Mousesdl.to_sdl_button Mouse.Middle);
  Sdl.Event.set event2 Sdl.Event.mouse_button_x 10;
  Sdl.Event.set event2 Sdl.Event.mouse_button_y 20;
  Sdl.Event.set event2 Sdl.Event.mouse_button_state Sdl.pressed;
  let mouse_after_middle, _ =
    Mousesdl.handle_mouse_button_event event2 mouse_after_left
  in
  (* Assert both buttons are pressed *)
  assert_bool "Expected Left button to be pressed"
    (Mouse.is_button_pressed mouse_after_middle Mouse.Left);
  assert_bool "Expected Middle button to be pressed"
    (Mouse.is_button_pressed mouse_after_middle Mouse.Middle);
  (* Simulate a drag motion *)
  Sdl.Event.set event Sdl.Event.mouse_motion_x 30;
  Sdl.Event.set event Sdl.Event.mouse_motion_y 40;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_motion;
  let _mouse_after_drag, events =
    Mousesdl.handle_mouse_motion_event event mouse_after_middle
  in
  (* Check drag events for both Left and Middle buttons *)
  assert_bool "Expected Drag event for Left button"
    (List.exists
       (function Event.MouseDrag (Mouse.Left, (30, 40)) -> true | _ -> false)
       events);
  assert_bool "Expected Drag event for Middle button"
    (List.exists
       (function
         | Event.MouseDrag (Mouse.Middle, (30, 40)) -> true | _ -> false)
       events)

let suite =
  "Mousesdl"
  >::: [
         "test_invalid_scale" >:: test_invalid_scale;
         "test_of_sdl_button" >:: test_of_sdl_button;
         "test_to_sdl_button" >:: test_to_sdl_button;
         "test_handle_mouse_button_event" >:: test_handle_mouse_button_event;
         "test_handle_mouse_motion_event" >:: test_handle_mouse_motion_event;
         "test_handle_mouse_wheel_event" >:: test_handle_mouse_wheel_event;
         "test_handle_event_button" >:: test_handle_event_button;
         "test_handle_event_motion" >:: test_handle_event_motion;
         "test_handle_event_wheel" >:: test_handle_event_wheel;
         "test_drag_for_all_buttons" >:: test_drag_for_all_buttons;
         "test_motion_after_drag_release" >:: test_motion_after_drag_release;
         "test_multiple_drag_events" >:: test_multiple_drag_events;
       ]

let () = run_test_tt_main suite
