(** Main Claudius entry point. *)

module KeyCodeSet : Set.S with type elt = Key.t
(** A module representing a set of key codes. *)

module PlatformKey : module type of Keysdl
(** A module that provides platform-specific key handling, based on the [Keysdl] module. *)

type boot_func = Screen.t -> Framebuffer.t
(** Function called once a start of run *)

(** A key event representing a key press or release. *)
type key_event =
  | KeyDown of Key.t  (** A key was pressed. *)
  | KeyUp of Key.t    (** A key was released. *)

(** The complete input state provided to each tick:
    - [pressed] 
    - [events]  *)

type input_state = {
  pressed : KeyCodeSet.t;
  events : key_event list;
}

type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
(** [run title boot tick screen] Creates the runloop *)