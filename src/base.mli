 
 (** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

module PlatformMouse : module type of Mousesdl
(** A module that provides platform-specific mouse handling, based on the {!Mousesdl} module. *)

(** FPS Calculation and Stats Display *)
val fps_counter : int ref
(** The current frames per second count *)

val stats_toggle_key : Key.t
(** Key to toggle stats display *)

val show_stats : bool ref
(** Whether stats display is currently enabled *)

val calculate_fps : unit -> unit
(** Calculate and update the FPS counter *)

val render_stats :
  (int -> int -> int -> 'a list -> 'a list) ->  (* create_pixel function *)
  int ->                                       (* width *)
  int ->                                       (* height *)
  'a list                                      (* returns a list of primitives *)
(** Render stats with a create_pixel function that builds primitives *)

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

type functional_tick_func = int -> Screen.t -> KeyCodeSet.t -> Primitives.t list

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)

val run_functional : string -> functional_tick_func -> Screen.t -> unit
(** [run_functional title tick_f screen] runs Claudius in a functional style.
  - [tick_f] screen returns a list of primitives rather than a complete framebuffer.*)