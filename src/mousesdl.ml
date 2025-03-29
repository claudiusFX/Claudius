open Mouse
open Tsdl

let of_sdl_button (button: int) : button =
  match button with
  | 1 -> Left
  | 2 -> Middle
  | 3 -> Right
  | _ -> Left  (* if an unknown button is pressed, then left button will be considered *)

let to_sdl_button (button: button) : int =
  match button with
  | Left -> 1
  | Middle -> 2
  | Right -> 3

let handle_mouse_button_event (event: Sdl.event) (t: t) : t =
  let button = of_sdl_button (Sdl.Event.get event Sdl.Event.mouse_button_button) in
  let x = Sdl.Event.get event Sdl.Event.mouse_button_x in
  let y = Sdl.Event.get event Sdl.Event.mouse_button_y in
  let pressed = (Sdl.Event.get event Sdl.Event.mouse_button_state) = Sdl.pressed in
  
  let t = update_button t button pressed in
  let t = update_position t (x, y) in
  let event = if pressed then Button_down (button, (x, y)) else Button_up (button, (x, y)) in
  add_event t event

let handle_mouse_motion_event (event: Sdl.event) (t: t) : t =
  let x = Sdl.Event.get event Sdl.Event.mouse_motion_x in
  let y = Sdl.Event.get event Sdl.Event.mouse_motion_y in
  let t = update_position t (x, y) in
  add_event t (Motion (x, y))

let handle_mouse_wheel_event (event: Sdl.event) (t: t) : t =
  let y = Sdl.Event.get event Sdl.Event.mouse_wheel_y in
  add_event t (Wheel y)

let handle_event (event: Sdl.event) (t: t) : t =
  let event_type = Sdl.Event.get event Sdl.Event.typ in
  if event_type = Sdl.Event.mouse_button_down || event_type = Sdl.Event.mouse_button_up then
    handle_mouse_button_event event t
  else if event_type = Sdl.Event.mouse_motion then
    handle_mouse_motion_event event t
  else if event_type = Sdl.Event.mouse_wheel then
    handle_mouse_wheel_event event t
  else
    t