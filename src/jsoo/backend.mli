open Claudius

type t

val v : Screen.t -> string -> bool -> (t, [ `Msg of string ]) result
(* Create a backend instance *)

val shutdown : t -> unit
(* Destroy a backend instance *)

val render : t -> Screen.t -> Framebuffer.t -> (unit, [ `Msg of string ]) result
(* Render a bitmap to the backend *)
