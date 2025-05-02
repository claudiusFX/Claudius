open OUnit2
open Claudius
open Tsdl

let test_empty_event _ =
  (* create the event for test *)
  let event = Sdl.Event.create () in
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;

  (* Set up other bits *)
  let mouse = Mouse.create 1
  and keys = Base.KeyCodeSet.empty in

  (* call the function we're testing *)
  let quit, updated_keys, updated_mouse, events = Base.poll_all_events keys mouse [] in

  (* check the results *)
  assert_equal ~printer:string_of_bool ~msg:"Quit result" false quit;
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false (Mouse.is_button_pressed updated_mouse Left);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Middle);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Right);
  assert_equal ~printer:string_of_int ~msg:"Events updated" 0 (List.length events);
  assert_equal ~printer:string_of_int ~msg:"Key code set" 0 (List.length (Base.KeyCodeSet.to_list updated_keys))

let test_key_down _ =
  (* create the event for test *)
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_down;
  Sdl.Event.set event Sdl.Event.keyboard_state Sdl.pressed;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.A);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;

  (* Set up other bits *)
  let mouse = Mouse.create 1
  and keys = Base.KeyCodeSet.empty in

  (* call the function we're testing *)
  let quit, updated_keys, updated_mouse, events = Base.poll_all_events keys mouse [] in

  (* check the results *)
  assert_equal ~printer:string_of_bool ~msg:"Quit result" false quit;
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false (Mouse.is_button_pressed updated_mouse Left);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Middle);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Right);

  let expected_key_code = Key.A in
  match events with
    | event :: [] -> (
      match event with
        | Event.KeyDown k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" expected_key_code k
        | _ -> ignore(assert_failure "Exected keydown")
    )
    | _ -> ignore(assert_failure "Expected event");
  let keycodes = Base.KeyCodeSet.to_list updated_keys in
  match keycodes with
    | k :: [] -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" expected_key_code k
    | _ -> ignore(assert_failure "Exected single key")

let test_key_up _ =
  (* create the event for test *)
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_up;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.A);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;

  (* Set up other bits *)
  let mouse = Mouse.create 1
  and keys = Base.KeyCodeSet.add Key.A Base.KeyCodeSet.empty in

  (* call the function we're testing *)
  let quit, updated_keys, updated_mouse, events = Base.poll_all_events keys mouse [] in

  (* check the results *)
  assert_equal ~printer:string_of_bool ~msg:"Quit result" false quit;
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false (Mouse.is_button_pressed updated_mouse Left);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Middle);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Right);

  let expected_key_code = Key.A in
  match events with
    | event :: [] -> (
      match event with
        | Event.KeyUp k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" expected_key_code k
        | _ -> ignore(assert_failure "Exected key up")
    )
    | _ -> ignore(assert_failure "Expected event");
  let keycodes = Base.KeyCodeSet.to_list updated_keys in
  match keycodes with
    | [] -> ()
    | _ -> ignore(assert_failure "Exected no keys")

let test_key_down_and_up _ =
  (* create the event for test *)
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_down;
  Sdl.Event.set event Sdl.Event.keyboard_state Sdl.pressed;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.A);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_up;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.A);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;

  (* Set up other bits *)
  let mouse = Mouse.create 1
  and keys = Base.KeyCodeSet.empty in

  (* call the function we're testing *)
  let quit, updated_keys, updated_mouse, events = Base.poll_all_events keys mouse [] in

  (* check the results *)
  assert_equal ~printer:string_of_bool ~msg:"Quit result" false quit;
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false (Mouse.is_button_pressed updated_mouse Left);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Middle);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Right);

  let expected_key_code = Key.A in
  match events with
    | event1 :: event2 :: [] -> (
      match event1 with
      | Event.KeyDown k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" expected_key_code k
      | _ -> ignore(assert_failure "Exected key down");
      match event2 with
        | Event.KeyUp k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" expected_key_code k
        | _ -> ignore(assert_failure "Exected key up")
    )
    | _ -> ignore(assert_failure "Expected event");
  let keycodes = Base.KeyCodeSet.to_list updated_keys in
  match keycodes with
    | [] -> ()
    | _ -> ignore(assert_failure "Exected no keys")

let test_mixed_key_down_and_up _ =
  (* create the event for test *)
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_down;
  Sdl.Event.set event Sdl.Event.keyboard_state Sdl.pressed;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.B);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;
  let event = Sdl.Event.create () in
  Sdl.Event.set event Sdl.Event.typ Sdl.Event.key_up;
  Sdl.Event.set event Sdl.Event.keyboard_keycode (Keysdl.to_backend_keycode Key.A);
  let success = Sdl.push_event event in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to push event: %s %s" e (Sdl.get_error ()))
    | Ok suc -> assert_equal true suc;

  (* Set up other bits *)
  let mouse = Mouse.create 1
  and keys = Base.KeyCodeSet.add Key.A Base.KeyCodeSet.empty in

  (* call the function we're testing *)
  let quit, updated_keys, updated_mouse, events = Base.poll_all_events keys mouse [] in

  (* check the results *)
  assert_equal ~printer:string_of_bool ~msg:"Quit result" false quit;
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false (Mouse.is_button_pressed updated_mouse Left);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Middle);
  assert_equal ~printer:string_of_bool ~msg:"No mouse buttons" false
  (Mouse.is_button_pressed updated_mouse Right);

  match events with
    | event1 :: event2 :: [] -> (
      match event1 with
      | Event.KeyDown k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" Key.B k
      | _ -> ignore(assert_failure "Exected key down");
      match event2 with
        | Event.KeyUp k -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" Key.A k
        | _ -> ignore(assert_failure "Exected key up")
    )
    | _ -> ignore(assert_failure "Expected event");
  let keycodes = Base.KeyCodeSet.to_list updated_keys in
  match keycodes with
    | k :: [] -> assert_equal ~printer:(fun k -> Int.to_string (Keysdl.to_backend_keycode k)) ~msg:"key code" Key.B k
    | _ -> ignore(assert_failure "Exected single key")

let suite =
  "Events tests" >::: [
  "Test empty event" >:: test_empty_event ;
  "Test a key down event" >:: test_key_down ;
  "Test a key up event" >:: test_key_up ;
  "Test a key down and up event" >:: test_key_down_and_up ;
  "Test a mix of key events" >:: test_mixed_key_down_and_up ;
  ]

let () =
  let success = Sdl.init Sdl.Init.(events) in
  match success with
    | Error (`Msg e) -> failwith (Printf.sprintf "failed to init SDL: %s %s" e (Sdl.get_error ()))
    | Ok _ -> ();
  run_test_tt_main suite
