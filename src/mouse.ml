(* mouse input *)

type button = Left | Middle | Right  (** Represents a mouse button. *)

type t = {
  position : int * int;
  buttons : (button * bool) list; (* current state of each button *)
  scale : int; (* scale factor for coordinates *)
}

let create scale =
  if scale <= 0 then invalid_arg "Invalid scale"
  else
    {
      position = (0, 0);
      buttons = [ (Left, false); (Middle, false); (Right, false) ];
      scale;
    }

let update_position t (x, y) = { t with position = (x / t.scale, y / t.scale) }

let update_button t button pressed =
  {
    t with
    buttons =
      List.map
        (fun (b, state) -> if b = button then (b, pressed) else (b, state))
        t.buttons;
  }

let is_button_pressed t button = List.assoc button t.buttons
let get_position t = t.position
let get_scale t = t.scale
