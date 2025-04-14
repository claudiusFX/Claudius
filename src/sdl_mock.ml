module MockSDL : SDL_API = struct

  (*Here, we define abstract types as trivial. For test, since real values are not used, we use [unit] 
    and for events, [string] since events are represented as string*)

  type window = unit
  type event = string
  type renderer = unit
  type texture = unit

  let init _width _height _title _fullscreen = Ok ((),())
  (*Ignores parameter and return a dummy window.*)

  let create_texture _renderer _format ~width:_ ~height:_ _access = Ok ()
  (*Ignores parameter and returns dummy texture*)

  let update_texture _renderer _texture _bitmap _width = Ok ()

  let renderer_clear _renderer = Ok ()

  let renderer_copy _renderer _texture ~dist:_ = Ok ()

  let renderer_present _renderer = Ok ()

  let poll_event () = None

  let delay _ = ()

  let log s = Printf.printf "MockSDL.log: %s\n" s
end