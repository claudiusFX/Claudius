open Claudius

val run : string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)

val run_functional : string -> functional_tick_func -> Screen.t -> unit
(** [run_functional title tick_f screen] runs Claudius in a functional style.
    - [tick_f] screen returns a list of primitives rather than a complete
      framebuffer.*)
