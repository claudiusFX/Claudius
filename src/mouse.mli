(** Mouse input handling. *)

(** Represents a mouse button. *)
type button =
  | Left
  | Middle
  | Right

(** Represents a mouse event. *)
type event =
  | Button_down of button * (int * int)     (** Button press event with position *)
  | Button_up of button * (int * int)       (** Button release event with position *)
  | Motion of (int * int)                   (** Mouse movement event *)
  | Wheel of int                            (** Scroll wheel event *)

(** Abstract type representing the mouse state. *)
type t

(** [create ()] initializes a new mouse state. *)
val create : unit -> t

(** [clear_event t] clears all stored mouse events. This is called every tick to prevent memory issues. *)
val clear_event : t -> t

(** [add_event t e] adds a new event [e] to the mouse state [t]. *)
val add_event : t -> event -> t

(** [update_position t (x, y)] updates the mouse position to [(x, y)]. *)
val update_position : t -> (int * int) -> t

(** [update_button t b state] updates the state of button [b] (pressed or released). *)
val update_button : t -> button -> bool -> t

(** [is_button_pressed t b] checks if button [b] is currently pressed. *)
val is_button_pressed : t -> button -> bool

(** [get_position t] returns the current mouse position. *)
val get_position : t -> (int * int)

(** [get_events t] retrieves the list of recorded mouse events. Events are cleared every tick, so this must be called within the same tick they occur. *)
val get_events : t -> event list 