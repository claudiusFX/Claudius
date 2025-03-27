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

  (* Alphabet keys (uppercase) *)
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

  (* Alphabet keys (lowercase) *)
  | A_upper
  | B_upper
  | C_upper
  | D_upper
  | E_upper
  | F_upper
  | G_upper
  | H_upper
  | I_upper
  | J_upper
  | K_upper
  | L_upper
  | M_upper
  | N_upper
  | O_upper
  | P_upper
  | Q_upper
  | R_upper
  | S_upper
  | T_upper
  | U_upper
  | V_upper
  | W_upper
  | X_upper
  | Y_upper
  | Z_upper

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