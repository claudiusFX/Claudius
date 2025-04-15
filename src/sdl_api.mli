open Tsdl

module type SDL_API = sig
(*These types will represents abstract sdl object*)
  type window
  type renderer
  type texture
  type event

  type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

  (**This handle the initialization of SDL
    parameters width, height, window title and fullscreen flag and onsuccess, 
    this returns a window and a renderer and on failure we have an error message
  *)
  val init : int -> int -> string -> bool -> (window * renderer, string) Stdlib.result

(* Create a texture, given a renderer, a pixel format and dimension of width and
   height if successful, a texture is returned else an error*)
  val create_texture : renderer -> int -> width:int -> height:int -> Sdl.Texture.access -> (texture, string) Stdlib.result

 (* Update texture with pixel data from [bitmap] the integer parameter, represents pixel width *)
  val update_texture : renderer -> texture -> bitmap_t -> int -> (unit, string) Stdlib.result

  (* [renderer_clear renderer] clears the renderer’s drawing target.
      Returns unit on success.
  *)
  val renderer_clear : renderer -> (unit, string) Stdlib.result

  (* [renderer_copy renderer texture dst] copies the [texture] to the renderer’s target.
      [dst] is an optional destination rectangle.
  *)
  val renderer_copy : renderer -> texture -> dst:Sdl.rect option -> (unit, string) Stdlib.result
end
