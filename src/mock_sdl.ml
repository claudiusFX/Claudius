module MockSDL : Sdl_api.SDL_API = struct
  (* Define abstract types as [unit] since no real values are needed during tests. *)
  type window = unit
  type renderer = unit
  type texture = unit
  type event = string  (* We'll represent events simply as strings in testing *)

  type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

  (* Simulate initializing SDL by ignoring input parameters and returning a dummy window and renderer. *)
  let init _width _height _title _fullscreen = Ok ((), ())

  (* Simulate texture creation by returning a dummy texture. *)
  let create_texture _renderer _format ~width:_ ~height:_ _access = Ok ()

  (* Simulate texture updating by doing nothing and returning success. *)
  let update_texture _renderer _texture _bitmap _width = Ok ()

  (* Simulate clearing the renderer. *)
  let renderer_clear _renderer = Ok ()

  (* Simulate copying a texture to the renderer's target. *)
  let renderer_copy _renderer _texture ~dst:_ = Ok ()
end
