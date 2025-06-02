(** This module provides a mapping between Claudius's key representation and the
    backend-specific integer keycodes used by SDL. *)

val of_backend_keycode : int -> Key.t
(** [of_backend_keycode keycode] converts a backend-specific integer keycode
    into a corresponding [Key.t] representation. *)

val to_backend_keycode : Key.t -> int
(** [to_backend_keycode key] converts a [Key.t] representation into a
    backend-specific integer keycode. *)
