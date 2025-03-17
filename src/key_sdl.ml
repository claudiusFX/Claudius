(* sdl keycodes *)
module KeySDL = struct
  let of_backend_keycode (keycode: int) : Key.t =
    match keycode with
    | 0x4000004F -> Key.Right
    | 0x40000050 -> Key.Left
    | 0x40000051 -> Key.Down
    | 0x40000052 -> Key.Up
    | 0x00000020 -> Key.Space
    | 0x0000001B -> Key.Escape
    | _ -> Key.Unknown

  let to_backend_keycode (key: Key.t) : int =
    match key with
    | Key.Right -> 0x4000004F
    | Key.Left -> 0x40000050
    | Key.Down -> 0x40000051
    | Key.Up -> 0x40000052
    | Key.Space -> 0x00000020
    | Key.Escape -> 0x0000001B
    | Key.Unknown -> -1
end