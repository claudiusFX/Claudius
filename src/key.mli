(** The Key module defines a platform-independent representation of keyboard keys. *)

type t =
  | Left
  | Right
  | Up
  | Down
  (** Arrow keys *)

  | Shift_L
  | Shift_R
  | Control_L
  | Control_R
  | Alt_L
  | Alt_R
  | CapsLock
  | NumLock
  | ScrollLock
  (** Modifier keys *)

  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  (** Function keys *)

  | Insert
  | Delete
  | Home
  | End
  | PageUp
  | PageDown
  (** Navigation keys *)

  | Space
  | Escape
  | Enter
  | Backspace
  | Tab
  | PrintScreen
  | Pause
  (** Special keys *)

  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  (** Alphabet keys (lowercase) *)

  | Num0
  | Num1
  | Num2
  | Num3
  | Num4
  | Num5
  | Num6
  | Num7
  | Num8
  | Num9
  (** Number keys *)

  | Unknown
  (** Unknown key *)

(** Type representing generic keys across different backends. *)