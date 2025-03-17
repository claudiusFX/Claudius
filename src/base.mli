(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = int

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

type tick_func = int -> Screen.t -> Framebuffer.t -> KeyCodeSet.t -> Framebuffer.t
(** Function called once a frame during run *)

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)
