open Claudius

type input_state = {
  keys : Key.KeyCodeSet.t;
  events : Event.t list;
      (** Accumulated unified input events for the current frame. *)
  mouse : Mouse.t;
}

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
