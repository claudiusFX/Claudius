(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the {!Mousesdl} module. *)

type key_event =
  | KeyDown of Key.t  (** A key was pressed. *)
  | KeyUp of Key.t    (** A key was released. *)

type input_state = {
  keys: KeyCodeSet.t;
  events: key_event list;
  mouse: Mouse.t;
}
(** Represents the current state of user input, including:
    - [keys]: The set of currently pressed keys.
    - [events]: The list of keyboard events that occurred this frame.
    - [mouse]: The current state of the mouse, including position and button presses. *)

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
(** Function called once a frame during run *)

type functional_tick_func = int -> Screen.t -> KeyCodeSet.t -> Primitives.t list

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)

val run_functional : string -> functional_tick_func -> Screen.t -> unit
(** [run_functional title tick_f screen] runs Claudius in a functional style. 
- [tick_f] screen returns a list of primitives rather than a complete framebuffer.*)