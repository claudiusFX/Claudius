(** Manages stats and other metadata for Claudius *)

type t

val create : unit -> t
(** Create an initial stats value *)

val fps : t -> int
(** Get current estimated FPS *)

val update : now:float -> tick:int -> t -> t
(** Calculate the updated stats based on current time/tick *)

val render : t -> int -> Screen.t -> Framebuffer.t -> unit
(** Draw stats on the provided framebuffer *)
