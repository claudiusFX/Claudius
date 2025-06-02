open Tsdl
open Mouse
open Event

let of_sdl_button (button : int) : button =
  match button with 1 -> Left | 2 -> Middle | 3 -> Right | _ -> Left
(* if an unknown button is pressed, then left button will be considered *)

let to_sdl_button (button : button) : int =
  match button with Left -> 1 | Middle -> 2 | Right -> 3

(** Handles an SDL mouse button event. Returns the updated mouse state and a
    list of unified events. *)
let handle_mouse_button_event (event : Sdl.event) (m : Mouse.t) :
    Mouse.t * Event.t list =
  let button =
    of_sdl_button (Sdl.Event.get event Sdl.Event.mouse_button_button)
  in
  let x = Sdl.Event.get event Sdl.Event.mouse_button_x in
  let y = Sdl.Event.get event Sdl.Event.mouse_button_y in
  let pressed =
    Sdl.Event.get event Sdl.Event.mouse_button_state = Sdl.pressed
  in
  let m = Mouse.update_button m button pressed in
  let m = Mouse.update_position m (x, y) in
  if pressed then
    ( m,
      [
        MouseButtonDown (button, (x / Mouse.get_scale m, y / Mouse.get_scale m));
      ] )
  else
    ( m,
      [ MouseButtonUp (button, (x / Mouse.get_scale m, y / Mouse.get_scale m)) ]
    )

(** Handles an SDL mouse motion event. Returns the updated mouse state and a
    list of unified events. *)
let handle_mouse_motion_event (event : Sdl.event) (m : Mouse.t) :
    Mouse.t * Event.t list =
  let x = Sdl.Event.get event Sdl.Event.mouse_motion_x in
  let y = Sdl.Event.get event Sdl.Event.mouse_motion_y in
  let m = Mouse.update_position m (x, y) in
  let buttons = [ Left; Middle; Right ] in
  let pressed_buttons =
    List.filter (fun b -> Mouse.is_button_pressed m b) buttons
  in
  if pressed_buttons = [] then
    (m, [ MouseMotion (x / Mouse.get_scale m, y / Mouse.get_scale m) ])
  else
    let drag_events =
      List.map
        (fun b -> MouseDrag (b, (x / Mouse.get_scale m, y / Mouse.get_scale m)))
        pressed_buttons
    in
    (m, drag_events)

(** Handles an SDL mouse wheel event. Returns the updated mouse state and a list
    containing the wheel event. *)
let handle_mouse_wheel_event (event : Sdl.event) (m : Mouse.t) :
    Mouse.t * Event.t list =
  let y = Sdl.Event.get event Sdl.Event.mouse_wheel_y in
  (m, [ MouseWheel y ])

(** Process any SDL mouse-related event. Returns the updated mouse state and any
    unified events. *)
let handle_event (event : Sdl.event) (m : Mouse.t) : Mouse.t * Event.t list =
  let event_type = Sdl.Event.get event Sdl.Event.typ in
  if
    event_type = Sdl.Event.mouse_button_down
    || event_type = Sdl.Event.mouse_button_up
  then handle_mouse_button_event event m
  else if event_type = Sdl.Event.mouse_motion then
    handle_mouse_motion_event event m
  else if event_type = Sdl.Event.mouse_wheel then
    handle_mouse_wheel_event event m
  else (m, [])
