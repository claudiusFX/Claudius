(* key.mli *)

(** The Key module defines a platform-independent representation of keyboard keys. *)

(** Type representing generic keys across different backends. *)
type t =
  | Left
  | Right
  | Up
  | Down
  | Space
  | Escape
  | Unknown