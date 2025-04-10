(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the {!Mousesdl} module. *)

(** Unified event type for both keyboard and mouse events. *)
type event =
  | KeyDown of Key.t           (** A key was pressed. *)
  | KeyUp of Key.t             (** A key was released. *)
  | MouseButtonDown of int     (** A mouse button was pressed (button code). *)
  | MouseButtonUp of int       (** A mouse button was released (button code). *)
  | MouseMotion of { x: int; y: int; xrel: int; yrel: int }  (** The mouse moved. *)
  | MouseWheel of { x: int; y: int }                        (** The mouse wheel was scrolled. *)

type input_state = {
  keys: KeyCodeSet.t;
  events: event list;  (** Accumulated input events for the current frame. *)
  mouse: Mouse.t;
}
(** Represents the current state of user input, including:
    - [keys]: The set of currently pressed keys.
    - [events]: A list of input events (keyboard and mouse) that occurred this frame.
    - [mouse]: The current state of the mouse, including position and button presses. *)

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
(** Function called once per frame during run *)

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list
(** A functional-style tick function that returns a list of primitives *)

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)

val run_functional : string -> functional_tick_func -> Screen.t -> unit
(** [run_functional title tick_f screen] runs Claudius in a functional style. 
- [tick_f] screen returns a list of primitives rather than a complete framebuffer.*)