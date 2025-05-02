(* event.ml *)
(* Unified event type. *)

type t =
  | KeyDown of Key.t
  | KeyUp of Key.t
  | MouseButtonDown of Mouse.button * (int * int)
  | MouseButtonUp of Mouse.button * (int * int)
  | MouseMotion of (int * int)
  | MouseWheel of int
  | MouseDrag of Mouse.button * (int * int)