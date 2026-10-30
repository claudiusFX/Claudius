open Claudius

val screen_of_canvas :
  ?font:Font.t -> ?scale:int -> string -> Palette.t -> Screen.t
(** Generate a new screen object from a named canvas element. *)

val run : ?boot:boot_func -> tick_func -> Screen.t -> unit
(** [run boot tick screen] Creates the runloop *)

val run_functional : functional_tick_func -> Screen.t -> unit
(** [run_functional tick_f screen] runs Claudius in a functional style.
    - [tick_f] screen returns a list of primitives rather than a complete
      framebuffer.*)
