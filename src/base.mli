(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the {!Mousesdl} module. *)

type input_state = {
  keys: KeyCodeSet.t;
  mouse: Mouse.t;
}
(** Represents the current state of user input, including:
    - [keys]: The set of currently pressed keys.
    - [mouse]: The current state of the mouse, including position and button presses. *)

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
(** Function called once a frame during run *)

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)