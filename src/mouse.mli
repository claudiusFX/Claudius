(** Mouse input handling. *)

type button =
  | Left
  | Middle
  | Right
  (** Represents a mouse button. *)

type event =
  | Button_down of button * (int * int)     (** Button press event with position *)
  | Button_up of button * (int * int)       (** Button release event with position *)
  | Motion of (int * int)                   (** Mouse movement event *)
  | Wheel of int                            (** Scroll wheel event *)
  (** Represents a mouse event. *)

type t
(** Abstract type representing the mouse state. *)

val create : int -> t
(** [create scale] initializes a new mouse state with a given scale factor. *)

val clear_event : t -> t
(** [clear_event t] clears all stored mouse events. This is called every tick to prevent memory issues. *)

val add_event : t -> event -> t
(** [add_event t e] adds a new event [e] to the mouse state [t], applying coordinate scaling. *)

val update_position : t -> (int * int) -> t
(** [update_position t (x, y)] updates the mouse position to [(x / scale, y / scale)]. *)

val update_button : t -> button -> bool -> t
(** [update_button t b state] updates the state of button [b] (pressed or released). *)

val is_button_pressed : t -> button -> bool
(** [is_button_pressed t b] checks if button [b] is currently pressed. *)

val get_position : t -> (int * int)
(** [get_position t] returns the current mouse position. *)

val get_events : t -> event list 
(** [get_events t] retrieves the list of recorded mouse events. Events are cleared every tick, so this must be called within the same tick they occur. *)