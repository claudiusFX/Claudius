(** Main Claudius entry point. *)

open Claudius

val run : ?boot:boot_func -> tick_func -> Screen.t -> unit
(** [run boot tick screen] Creates the runloop *)

val run_functional : functional_tick_func -> Screen.t -> unit
(** [run_functional tick_f screen] runs Claudius in a functional style.
    - [tick_f] screen returns a list of primitives rather than a complete
      framebuffer.*)
