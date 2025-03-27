(* keys *)

type t =
  (* Arrow keys *)
  | Left
  | Right
  | Up
  | Down

  (* Modifier keys *)
  | Shift_L
  | Shift_R
  | Control_L
  | Control_R
  | Alt_L
  | Alt_R
  | CapsLock
  | NumLock
  | ScrollLock

  (* Function keys *)
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

  (* Navigation keys *)
  | Insert
  | Delete
  | Home
  | End
  | PageUp
  | PageDown

  (* Special keys *)
  | Space
  | Escape
  | Enter
  | Backspace
  | Tab
  | PrintScreen
  | Pause

  (* Alphabet keys (lowercase) *)
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

  (* Number keys *)
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

  (* Unknown key *)
  | Unknown