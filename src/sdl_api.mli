open Tsdl

module type SDL_API = sig
  (*These types will represents abstract sdl object*)
  type window
  type renderer
  type texture
  type event

  
  val init : int -> int -> string -> bool -> (window * renderer, string) result
   (**This handle the initialization of SDL
    arguments width, height, window title and fullscreen flag and onsuccess, 
    this returns a window and a renderer and on failure we have an error message
  *)
  
  val create_texture : renderer -> Sdl.Pixel.format -> width:int -> height:int -> Sdl.Texture.access -> (texture, string) result
  (* Create a texture, given a renderer, a pixel format and dimension of width and
   height if successful, a texture is returned else an error*)
 
  val update_texture : renderer -> texture -> Bigarray.Array1.t -> int -> (unit, string) result
  (* Update texture with pixel data from BigArray the integer parameter, represents pixel width *)

  val renderer_clear : renderer -> (unit, string) result
  (* Clears renderer drawing target*)
 
  val renderer_copy : renderer -> texture -> Sdl.Rect.t option -> (unit, string) result
  (* Copies a texture to a renderer's target indicating where to place the texture*)

  val renderer_present : renderer -> (unit, string) result
  (* Present the content of the renderer to the sting.*)

  val poll_event : unit -> event option
  (* Polls an SDL event returns event if available*)

  val delay : int -> unit
  (* delays execution for a number of milliseconds*)

  val log : string -> unit
  (*logs messages *)
end