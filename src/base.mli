(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl]
    module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the
    {!Mousesdl} module. *)

val show_stats : bool ref
(** Whether stats display is currently enabled *)

type input_state = {
  keys : KeyCodeSet.t;
  events : Event.t list;
      (** Accumulated unified input events for the current frame. *)
  mouse : Mouse.t;
}
(** Represents the current state of user input, including:
    - [keys]: The set of currently pressed keys.
    - [mouse]: The current state of the mouse, including position and button
      presses. *)

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

type tick_func =
  int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
(** Function called once a frame during run *)

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list
(** A functional-style tick function that returns a list of primitives. *)

val run : string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)

val run_functional : string -> functional_tick_func -> Screen.t -> unit
(** [run_functional title tick_f screen] runs Claudius in a functional style.
    - [tick_f] screen returns a list of primitives rather than a complete
      framebuffer.*)

(* --- Utility function signatures for input handling --- *)

val is_key_pressed : input_state -> Key.t -> bool
(** Returns [true] if [key] is currently pressed. *)

val was_key_just_pressed : input_state -> Key.t -> bool
(** Returns [true] if [key] was pressed during the current frame. *)

val was_key_just_released : input_state -> Key.t -> bool
(** Returns [true] if [key] was released during the current frame. *)

val poll_all_events :
  KeyCodeSet.t ->
  Mouse.t ->
  Event.t list ->
  bool * KeyCodeSet.t * Mouse.t * Event.t list
(** Internal method exposed for testing *)
