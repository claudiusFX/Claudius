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

let render fps_stats screen framebuffer =
  let width, height = Screen.dimensions screen and font = Screen.font screen in

  let fps_text = Printf.sprintf "FPS: %d" fps_stats.average_fps in
  let res_text = Printf.sprintf "RES: %dx%d" width height in

  let base_x = 4 in
  let base_y = 4 in

  let palette_max = Palette.size (Screen.palette screen) - 1 in

  ignore
    (Framebuffer.draw_string base_x base_y font fps_text palette_max framebuffer);
  ignore
    (Framebuffer.draw_string base_x (base_y + 14) font res_text palette_max
       framebuffer)
