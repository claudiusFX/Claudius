(* mouse input *)

type button =
  | Left
  | Middle
  | Right

type event =
  | Button_down of button * (int * int)  (* button pressed with coordinates *)
  | Button_up of button * (int * int)    (* button released with coordinates *)
  | Motion of (int * int)                (* mouse moved to coordinates *)
  | Wheel of int                         (* positive for upward scroll, negative for downward scroll *)

type t = {
  events: event list;
  position: (int * int);
  buttons: (button * bool) list;         (* current state of each button *)
  scale: int;                            (* scale factor for coordinates *)
}

let create scale = 
  if scale <= 0 then
    invalid_arg "Invalid scale";
{
  events = [];
  position = (0, 0);
  buttons = [(Left, false); (Middle, false); (Right, false)];
  scale;
}

let clear_events t = {
  t with
  events = [];
}

let add_event t event = 
  let scale_coords (x, y) = (x / t.scale, y / t.scale) in
  let scaled_event = match event with
    | Button_down (b, (x, y)) -> Button_down (b, scale_coords (x, y))
    | Button_up (b, (x, y)) -> Button_up (b, scale_coords (x, y))
    | Motion (x, y) -> Motion (scale_coords (x, y))
    | Wheel _ as e -> e                   (* Wheel events remain unchanged *)
  in
{
  t with
  events = scaled_event :: t.events;
}

let update_position t (x, y) = {
  t with
  position = (x / t.scale, y / t.scale);
}

let update_button t button pressed = {
  t with
  buttons = List.map (fun (b, state) ->
    if b = button then (b, pressed) else (b, state)
  ) t.buttons;
}

let is_button_pressed t button =
  List.assoc button t.buttons

let get_position t = t.position

let get_events t = List.rev t.events 