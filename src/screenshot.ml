open Giflib
open Utils_gif

let save_screenshot (events : Event.t list) (screen : Screen.t)
    (fb : Framebuffer.t) =
  let take_screenshot =
    List.fold_left
      (fun acc ev -> match ev with Event.KeyDown Key.F2 -> true | _ -> acc)
      false events
  in

  if take_screenshot then (
    if Palette.size (Screen.palette screen) > 256 then
      failwith "GIF only supports up to 256 colors";

    let image = capture_frame screen fb in
    let gif = GIF.from_image image in
    let filename = timestamp "screenshot" ^ ".gif" in
    GIF.to_file gif filename;
    Printf.printf "Screenshot saved as %s\n%!" filename)
