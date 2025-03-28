open OUnit2
open Claudius
open Tsdl

let setup () =
  (Sdl.Event.create (), Mouse.create ())

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
  let event, mouse = setup () in
  
  (* Test button down *)
  Sdl.Event.set event Sdl.Event.mouse_button_button 1;
  Sdl.Event.set event Sdl.Event.mouse_button_x 100;
  Sdl.Event.set event Sdl.Event.mouse_button_y 200;
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;

  let mouse = Mousesdl.handle_mouse_button_event event mouse in
  assert_equal (100, 200) (Mouse.get_position mouse);
  assert_equal true (Mouse.is_button_pressed mouse Mouse.Left);

  (* Test button up *)
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.released;
  let mouse = Mousesdl.handle_mouse_button_event event mouse in
  assert_equal false (Mouse.is_button_pressed mouse Mouse.Left)

let test_handle_mouse_motion_event _ =
  let event, mouse = setup () in
  
  Sdl.Event.set event Sdl.Event.mouse_motion_x 150;
  Sdl.Event.set event Sdl.Event.mouse_motion_y 250;
  
  let mouse = Mousesdl.handle_mouse_motion_event event mouse in
  assert_equal (150, 250) (Mouse.get_position mouse)

let test_handle_mouse_wheel_event _ =
  let event, mouse = setup () in

  Sdl.Event.set event Sdl.Event.mouse_wheel_y 1;  (* Scroll up *)
  let mouse = Mousesdl.handle_mouse_wheel_event event mouse in

  match Mouse.get_events mouse with
  | [Mouse.Wheel 1] -> ()
  | _ -> failwith "Expected Wheel event with positive value"

(* Split test_handle_event into three separate tests *)
let test_handle_event_button _ =
  let event, mouse = setup () in

  Sdl.Event.set event Sdl.Event.mouse_button_button 1;
  Sdl.Event.set event Sdl.Event.mouse_button_x 100;
  Sdl.Event.set event Sdl.Event.mouse_button_y 200;
  Sdl.Event.set event Sdl.Event.mouse_button_state Sdl.pressed;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_button_down;

  let mouse = Mousesdl.handle_event event mouse in
  assert_equal (100, 200) (Mouse.get_position mouse);
  assert_equal true (Mouse.is_button_pressed mouse Mouse.Left)

let test_handle_event_motion _ =
  let event, mouse = setup () in

  Sdl.Event.set event Sdl.Event.mouse_motion_x 150;
  Sdl.Event.set event Sdl.Event.mouse_motion_y 250;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_motion;

  let mouse = Mousesdl.handle_event event mouse in
  assert_equal (150, 250) (Mouse.get_position mouse)

let test_handle_event_wheel _ =
  let event, mouse = setup () in

  Sdl.Event.set event Sdl.Event.mouse_wheel_y 1;
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.mouse_wheel;

  let mouse = Mousesdl.handle_event event mouse in

  match Mouse.get_events mouse with
  | [Mouse.Wheel 1] -> ()
  | _ -> failwith "Expected Wheel event with positive value"

let suite =
  "Mousesdl" >::: [
    "test_of_sdl_button" >:: test_of_sdl_button;
    "test_to_sdl_button" >:: test_to_sdl_button;
    "test_handle_mouse_button_event" >:: test_handle_mouse_button_event;
    "test_handle_mouse_motion_event" >:: test_handle_mouse_motion_event;
    "test_handle_mouse_wheel_event" >:: test_handle_mouse_wheel_event;
    "test_handle_event_button" >:: test_handle_event_button;
    "test_handle_event_motion" >:: test_handle_event_motion;
    "test_handle_event_wheel" >:: test_handle_event_wheel;
  ]

let () = run_test_tt_main suite