type t = {
  last_update : float;
  last_tick_count : int;
  average_fps : int;
  log : (string * float) list;
}

let create () =
  { last_update = 0.0; last_tick_count = 0; average_fps = 0; log = [] }

let fps t = t.average_fps

let update ~now ~tick previous =
  let elapsed = now -. previous.last_update in
  if elapsed >= 1.0 then
    {
      last_update = now;
      last_tick_count = tick;
      average_fps = tick - previous.last_tick_count;
      log = previous.log;
    }
  else previous

let log t msg =
  let log = (msg, t.last_update) :: t.log in
  { t with log }

let draw_string x y font msg fg_col bg_col fb =
  for j = -1 to 1 do
    for i = -1 to 1 do
      ignore (Framebuffer.draw_string (x + i) (y + j) font msg bg_col fb)
    done
  done;
  ignore (Framebuffer.draw_string x y font msg fg_col fb)

let render_log messages screen framebuffer =
  let _, h = Screen.dimensions screen in
  let font = Screen.font screen in
  let pal = Screen.palette screen in
  let bg_col, fg_col = Palette.distinctive_pair pal in
  List.iteri
    (fun i (a, _) ->
      draw_string 10 (h - (20 + (i * 20))) font a fg_col bg_col framebuffer)
    messages

let render_stats status tick screen framebuffer =
  let width, height = Screen.dimensions screen
  and font = Screen.font screen
  and colour_count = Palette.size (Screen.palette screen)
  and bg_col, fg_col = Palette.distinctive_pair (Screen.palette screen) in
  let info =
    [
      ("Tick:", string_of_int tick);
      ("FPS:", string_of_int status.average_fps);
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
  done

let render status show_all tick screen framebuffer =
  let log_messages =
    match show_all with
    | false ->
        let log_threshold = status.last_update -. 5.0 in
        List.filter (fun (_, a) -> a > log_threshold) status.log
    | true -> status.log
  in
  let show_log = List.length log_messages > 0 in

  match (show_all, show_log) with
  | false, false -> None
  | _, _ ->
      let framebuffer = Framebuffer.map (fun i -> i) framebuffer in
      if show_all then render_stats status tick screen framebuffer;
      if show_log then render_log log_messages screen framebuffer;
      Some framebuffer
