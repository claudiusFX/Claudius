type t = { last_update : float; last_tick_count : int; average_fps : int }

let create () = { last_update = 0.0; last_tick_count = 0; average_fps = 0 }
let fps t = t.average_fps

let update ~now ~tick previous =
  let elapsed = now -. previous.last_update in
  if elapsed >= 1.0 then
    {
      last_update = now;
      last_tick_count = tick;
      average_fps = tick - previous.last_tick_count;
    }
  else previous

let draw_string x y font msg fg_col bg_col fb =
  for j = -1 to 1 do
    for i = -1 to 1 do
      ignore (Framebuffer.draw_string (x + i) (y + j) font msg bg_col fb)
    done
  done;
  ignore (Framebuffer.draw_string x y font msg fg_col fb)

let render fps_stats tick screen framebuffer =
  let framebuffer = Framebuffer.map (fun i -> i) framebuffer in
  let width, height = Screen.dimensions screen
  and font = Screen.font screen
  and colour_count = Palette.size (Screen.palette screen)
  and bg_col, fg_col = Palette.extremes (Screen.palette screen) in

  let info =
    [
      ("Tick:", string_of_int tick);
      ("FPS:", string_of_int fps_stats.average_fps);
      ("Resolution:", Printf.sprintf "%dx%d" width height);
      ("Colours:", string_of_int colour_count);
    ]
  in

  let max_key_width =
    List.fold_left
      (fun acc (k, _) ->
        let width =
          Framebuffer.draw_string (-1000) (-1000) font k 0 framebuffer
        in
        if width > acc then width else acc)
      0 info
  in

  List.iteri
    (fun i (k, v) ->
      let y_offset = 4 + (14 * i) in
      draw_string 4 y_offset font k fg_col bg_col framebuffer;
      draw_string (max_key_width + 10) y_offset font v fg_col bg_col framebuffer)
    info;

  let columns = width / 10 in
  let rows = (colour_count / columns) + 1 in
  let offset = height - (10 * rows) in
  for i = 0 to colour_count - 1 do
    Framebuffer.filled_rect
      (i mod columns * 10)
      (offset + (i / columns * 10))
      10 10 i framebuffer
  done;

  framebuffer
