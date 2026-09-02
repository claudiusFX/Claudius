open Claudius

type t

val v : Screen.t -> string -> bool -> (t, [ `Msg of string ]) result
(* Create a backend instance *)

val shutdown : t -> unit
(* Destroy a backend instance *)

val render : t -> Screen.t -> Framebuffer.t -> (unit, [ `Msg of string ]) result
(* Render a bitmap to the backend *)

val poll_all_events :
  Key.KeyCodeSet.t ->
  Mouse.t ->
  Event.t list ->
  bool * Key.KeyCodeSet.t * Mouse.t * Event.t list

val log : ('b, Stdlib.Format.formatter, unit) Stdlib.format -> 'b
val get_ticks : unit -> Int32.t
val delay : Int32.t -> unit
