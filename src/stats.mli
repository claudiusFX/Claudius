(** Manages stats and other metadata for Claudius *)

type t

val create : unit -> t
(** Create an initial stats value *)

val fps : t -> int
(** Get current estimated FPS *)

val update : now:float -> tick:int -> t -> t
(** Calculate the updated stats based on current time/tick *)

val log : t -> string -> t

val render :
  t -> bool -> int -> Screen.t -> Framebuffer.t -> Framebuffer.t option
(** Draw stats on the provided framebuffer *)
