(** SDL-based mouse handling for Claudius. *)


open Mouse
open Tsdl

val of_sdl_button : int -> button
(** Convert an SDL button code to a [Mouse.button]. *)

val to_sdl_button : button -> int
(** Convert a [Mouse.button] to its SDL representation. *)

val handle_mouse_button_event : Sdl.event -> t -> t
(** Handle an SDL mouse button event and update the mouse state. *)

val handle_mouse_motion_event : Sdl.event -> t -> t
(** Handle an SDL mouse motion event and update the mouse position. *)

val handle_mouse_wheel_event : Sdl.event -> t -> t
(** Handle an SDL mouse wheel event and update the scroll state. *)

val handle_event : Sdl.event -> t -> t 
(** Process any SDL mouse-related event and update the mouse state. *)