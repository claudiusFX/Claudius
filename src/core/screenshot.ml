open Giflib
open Utils_gif

let save_screenshot (screen : Screen.t) (fb : Framebuffer.t) =
  match Palette.size (Screen.palette screen) > 256 with
  | true -> Result.Error "GIF only supports up to 256 colors"
  | false ->
      let image = capture_frame screen fb in
      let gif = GIF.from_image image in
      let filename = timestamp "screenshot" ^ ".gif" in
      GIF.to_file gif filename;
      Result.Ok filename
