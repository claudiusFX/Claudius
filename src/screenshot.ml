open Giflib
open Unix
open Utils_gif

let now_string () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "screenshot_%02d%02d%02d_%02d%02d%02d" tm.tm_mday
    (tm.tm_mon + 1) (tm.tm_year mod 100) tm.tm_hour tm.tm_min tm.tm_sec

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
    let filename = now_string () ^ ".gif" in
    GIF.to_file gif filename;
    Printf.printf "Screenshot saved as %s\n%!" filename
  )