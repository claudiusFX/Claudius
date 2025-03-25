(* mouse input *)

type button =
  | Left
  | Middle
  | Right

type event =
  | Button_down of button * (int * int)
  | Button_up of button * (int * int)
  | Motion of (int * int)
  | Wheel of int

type t

val create : unit -> t
val clear : t -> t
val add_event : t -> event -> t
val update_position : t -> (int * int) -> t
val update_button : t -> button -> bool -> t
val is_button_pressed : t -> button -> bool
val get_position : t -> (int * int)
val get_events : t -> event list 