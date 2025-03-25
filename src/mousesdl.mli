open Mouse
open Tsdl

val of_sdl_button : int -> button
val to_sdl_button : button -> int
val handle_mouse_button_event : Sdl.event -> t -> t
val handle_mouse_motion_event : Sdl.event -> t -> t
val handle_mouse_wheel_event : Sdl.event -> t -> t
val handle_event : Sdl.event -> t -> t 