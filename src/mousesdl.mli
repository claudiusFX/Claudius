(* mousesdl.mli *)
open Tsdl
open Mouse

val of_sdl_button : int -> button
(** Convert an SDL button code to a [Mouse.button]. *)

val to_sdl_button : button -> int
(** Convert a [Mouse.button] to its SDL representation. *)

val handle_mouse_button_event : Sdl.event -> t -> t * Event.t list
(** Handle an SDL mouse button event, updating the mouse state and returning
    unified events. *)

val handle_mouse_motion_event : Sdl.event -> t -> t * Event.t list
(** Handle an SDL mouse motion event, updating the mouse position and returning
    unified events. *)

val handle_mouse_wheel_event : Sdl.event -> t -> t * Event.t list
(** Handle an SDL mouse wheel event, updating the scroll state and returning
    unified events. *)

val handle_event : Sdl.event -> t -> t * Event.t list
(** Process any SDL mouse-related event, updating the mouse state and returning
    unified events. *)
