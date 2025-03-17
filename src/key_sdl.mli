module KeySDL : sig
  val of_backend_keycode : int -> Key.t
  val to_backend_keycode : Key.t -> int
end