open Giflib
open Unix

type recording_state = {
  mutable frames : Image.t list;
  mutable is_recording : bool;
  mutable frames_to_record : int;
  mutable current_frame : int;
}

let recording_state =
  { frames = []; is_recording = false; frames_to_record = 0; current_frame = 0 }

let now_string () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "animation_%02d%02d%02d_%02d%02d%02d" tm.tm_mday
    (tm.tm_mon + 1) (tm.tm_year mod 100) tm.tm_hour tm.tm_min tm.tm_sec

let color_table_of_palette (p : Palette.t) : ColorTable.t =
  Array.init (Palette.size p) (fun i ->
      let rgb32 = Palette.index_to_rgb p i in
      let rgb = Int32.to_int rgb32 in
      let r = (rgb lsr 16) land 0xFF in
      let g = (rgb lsr 8) land 0xFF in
      let b = rgb land 0xFF in
      (r, g, b))

let pad_palette_to_power_of_two (colors : ColorTable.t) : ColorTable.t =
  let len = Array.length colors in
  let next_pow2 n =
    let rec loop x = if x >= n then x else loop (x * 2) in
    loop 1
  in
  let target_len = min 256 (next_pow2 len) in
  Array.init target_len (fun i -> if i < len then colors.(i) else (0, 0, 0))

let capture_frame (screen : Screen.t) (fb : Framebuffer.t) =
  let width, height = Screen.dimensions screen in
  let scale = Screen.scale screen in
  let palette = Screen.palette screen in

  let scaled_width = width * scale in
  let scaled_height = height * scale in

  let size = scaled_width * scaled_height in

  let colors =
    palette |> color_table_of_palette |> pad_palette_to_power_of_two
  in

  let color_depth =
    let len = Array.length colors in
    let rec bits_needed n b =
      if n <= 1 then b else bits_needed (n / 2) (b + 1)
    in
    min 8 (max 2 (bits_needed (len - 1) 1))
  in

  let pixels =
    List.init size (fun idx ->
        let x = idx mod scaled_width in
        let y = idx / scaled_width in
        let src_x = x / scale in
        let src_y = y / scale in
        let v =
          match Framebuffer.pixel_read src_x src_y fb with
          | Some v -> v
          | None ->
              failwith
                (Printf.sprintf "Invalid pixel coordinate (%d,%d)" src_x src_y)
        in
        if v < 0 || v > Palette.size palette then
          failwith
            (Printf.sprintf "Framebuffer value %d out of byte range at (%d,%d)"
               v src_x src_y);
        (Z.of_int v, color_depth))
  in

  let flattened = Lzw.flatten_codes 8 pixels in
  let compressed = Lzw.encode flattened color_depth in

  Image.v (scaled_width, scaled_height) colors compressed color_depth true

let start_recording n =
  if recording_state.is_recording then failwith "Already recording animation";
  if n <= 0 then failwith "Number of frames must be positive";
  if n > 100 then (*Ask mdales what limit to put on this, currently set to 100*)
    failwith "Maximum 100 frames allowed";
  recording_state.is_recording <- true;
  recording_state.frames_to_record <- n;
  recording_state.current_frame <- 0;
  recording_state.frames <- [];
  Printf.printf "Started recording %d frames\n%!" n

let stop_recording () =
  if not recording_state.is_recording then failwith "Not recording animation";
  recording_state.is_recording <- false;
  let frames = List.rev recording_state.frames in

  let gif = GIF.from_images frames in
  let filename = now_string () ^ ".gif" in
  GIF.to_file gif filename;
  Printf.printf "Animation saved as %s\n%!" filename;
  recording_state.frames <- []

let record_frame (screen : Screen.t) (fb : Framebuffer.t) =
  if not recording_state.is_recording then ()
  else if recording_state.current_frame >= recording_state.frames_to_record then
    stop_recording ()
  else (
    if Palette.size (Screen.palette screen) > 256 then
      failwith "GIF only supports up to 256 colors";
    let frame = capture_frame screen fb in
    recording_state.frames <- frame :: recording_state.frames;
    recording_state.current_frame <- recording_state.current_frame + 1;
    if recording_state.current_frame = recording_state.frames_to_record then
      stop_recording ())

(* Note to self: We need to abstract a lot of functions here to utils_gif or so as they are being repeated here and in screenshot.ml*)
