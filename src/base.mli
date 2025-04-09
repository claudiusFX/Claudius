(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the {!Mousesdl} module. *)

(** Unified event type for input events. *)
type event =
  | KeyDown of Key.t  (** A key was pressed. *)
  | KeyUp of Key.t    (** A key was released. *)
  | MouseButtonDown of int  (** A mouse button was pressed (button code). *)
  | MouseButtonUp of int    (** A mouse button was released (button code). *)
  | MouseMotion of { x: int; y: int; xrel: int; yrel: int }  (** The mouse moved. *)
  | MouseWheel of { x: int; y: int }  (** The mouse wheel was scrolled. *)

(** Represents the current state of user input, including:
    - [keys]: The set of currently pressed keys.
    - [events]: The list of input events (keyboard and mouse) that occurred this frame.
    - [mouse]: The current state of the mouse, including button state and position. *)
type input_state = {
  keys: KeyCodeSet.t;
  events: event list;
  mouse: Mouse.t;
}

(** Function called once at start of run. *)
type boot_func = Screen.t -> Framebuffer.t

(** Function called once per frame during run.
    - The [int] parameter is the current frame count.
    - [Screen.t] is the current screen configuration.
    - [Framebuffer.t] is the current framebuffer.
    - [input_state] is the state of user input.
    Returns an updated framebuffer. *)
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
(** Function called once a frame during run *)

(** [run title boot tick screen] creates and runs the main loop.
    - [title]: The window title.
    - [boot]: An optional boot function that initializes the framebuffer.
    - [tick]: The per-frame function.
    - [screen]: The initial screen configuration. *)
val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)