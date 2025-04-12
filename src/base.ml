(* base.ml *)
open Tsdl

module KeyCodeSet = Set.Make(struct
  type t = Key.t
  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

(* FPS Calculation *)
let last_time = ref 0.0
let frame_count = ref 0
let fps_counter = ref 0

(* Stats toggle key *)
let stats_toggle_key = Key.F
let show_stats = ref false

(* Calculate FPS *)
let calculate_fps () =
  let now = Unix.gettimeofday () in
  let elapsed = now -. !last_time in
  frame_count := !frame_count + 1;
  if elapsed >= 1.0 then (
    fps_counter := !frame_count;
    frame_count := 0;
    last_time := now
  )

(* Font rendering - optimized to cache character patterns *)
let char_patterns = Hashtbl.create 36

let get_char_pattern c =
  match Hashtbl.find_opt char_patterns c with
  | Some pattern -> pattern
  | None ->
      let pattern = match c with
      | 'F' -> [
          (0, 0); (1, 0); (2, 0); (3, 0);
          (0, 1);
          (0, 2); (1, 2); (2, 2);
          (0, 3);
          (0, 4)
        ]
      | 'P' -> [
          (0, 0); (1, 0); (2, 0);
          (0, 1); (3, 1);
          (0, 2); (1, 2); (2, 2);
          (0, 3);
          (0, 4)
        ]
      | 'S' -> [
          (1, 0); (2, 0); (3, 0);
          (0, 1);
          (1, 2); (2, 2);
          (3, 3);
          (0, 4); (1, 4); (2, 4)
        ]
      | 'R' -> [
          (0, 0); (1, 0); (2, 0);
          (0, 1); (3, 1);
          (0, 2); (1, 2); (2, 2);
          (0, 3); (2, 3);
          (0, 4); (3, 4)
        ]
      | 'E' -> [
          (0, 0); (1, 0); (2, 0); (3, 0);
          (0, 1);
          (0, 2); (1, 2); (2, 2);
          (0, 3);
          (0, 4); (1, 4); (2, 4); (3, 4)
        ]
      | 'D' -> [
          (0, 0); (1, 0); (2, 0);
          (0, 1); (3, 1);
          (0, 2); (3, 2);
          (0, 3); (3, 3);
          (0, 4); (1, 4); (2, 4)
        ]
      | 'O' -> [
          (1, 0); (2, 0);
          (0, 1); (3, 1);
          (0, 2); (3, 2);
          (0, 3); (3, 3);
          (1, 4); (2, 4)
        ]
      | 'T' -> [
          (0, 0); (1, 0); (2, 0); (3, 0);
          (1, 1); (2, 1);
          (1, 2); (2, 2);
          (1, 3); (2, 3);
          (1, 4); (2, 4)
        ]
      | ':' -> [
          (1, 1); (2, 1);
          (1, 2); (2, 2);
          (1, 3); (2, 3)
        ]
      | 'x' -> [
          (0, 0); (3, 0);
          (1, 1); (2, 1);
          (1, 2); (2, 2);
          (1, 3); (2, 3);
          (0, 4); (3, 4)
        ]
      | ' ' -> []
      | '0' -> [
          (1, 0); (2, 0);
          (0, 1); (3, 1);
          (0, 2); (3, 2);
          (0, 3); (3, 3);
          (1, 4); (2, 4)
        ]
      | '1' -> [
          (1, 0);
          (0, 1); (1, 1);
          (1, 2);
          (1, 3);
          (0, 4); (1, 4); (2, 4)
        ]
      | '2' -> [
          (1, 0); (2, 0);
          (0, 1); (3, 1);
          (2, 2); (3, 2);
          (1, 3);
          (0, 4); (1, 4); (2, 4); (3, 4)
        ]
      | '3' -> [
          (0, 0); (1, 0); (2, 0);
          (3, 1);
          (1, 2); (2, 2);
          (3, 3);
          (0, 4); (1, 4); (2, 4)
        ]
      | '4' -> [
          (0, 0); (3, 0);
          (0, 1); (3, 1);
          (0, 2); (1, 2); (2, 2); (3, 2);
          (3, 3);
          (3, 4)
        ]
      | '5' -> [
          (0, 0); (1, 0); (2, 0); (3, 0);
          (0, 1);
          (0, 2); (1, 2); (2, 2);
          (3, 3);
          (0, 4); (1, 4); (2, 4)
        ]
      | '6' -> [
          (1, 0); (2, 0);
          (0, 1);
          (0, 2); (1, 2); (2, 2);
          (0, 3); (3, 3);
          (1, 4); (2, 4)
        ]
      | '7' -> [
          (0, 0); (1, 0); (2, 0); (3, 0);
          (3, 1);
          (2, 2);
          (1, 3);
          (1, 4)
        ]
      | '8' -> [
          (1, 0); (2, 0);
          (0, 1); (3, 1);
          (1, 2); (2, 2);
          (0, 3); (3, 3);
          (1, 4); (2, 4)
        ]
      | '9' -> [
          (1, 0); (2, 0);
          (0, 1); (3, 1);
          (1, 2); (2, 2); (3, 2);
          (3, 3);
          (1, 4); (2, 4)
        ]
      | _ -> []
      in
      Hashtbl.add char_patterns c pattern;
      pattern

let draw_char x y c brightness create_pixel acc =
  let pixels = get_char_pattern c in
  List.fold_left (fun acc (px, py) ->
    create_pixel ((x+px)*2) ((y+py)*2) brightness acc
  ) acc pixels

let draw_string x y text brightness create_pixel acc =
  let chars = List.of_seq (String.to_seq text) in
  let rec aux x y chars acc =
    match chars with
    | [] -> acc
    | c :: cs ->
      let new_acc = draw_char (x/2) (y/2) c brightness create_pixel acc in
      aux (x + 10) y cs new_acc
  in
  aux x y chars acc

let cached_fps_text = ref ""
let cached_fps_value = ref (-1)
let cached_res_text = ref ""
let cached_width = ref (-1)
let cached_height = ref (-1)

let render_stats create_pixel width height =
  if !fps_counter != !cached_fps_value then (
    cached_fps_text := Printf.sprintf "FPS: %d" !fps_counter;
    cached_fps_value := !fps_counter
  );

  if !cached_width != width || !cached_height != height then (
    cached_res_text := Printf.sprintf "RES: %dx%d" width height;
    cached_width := width;
    cached_height := height
  );

  (* let entity_text = Printf.sprintf "DOTS: %d" entity_count in *)

  let base_x = 2 in
  let base_y = 2 in
  let brightness = 15 in

  let fps_primitives = draw_string (base_x*2) (base_y*2) !cached_fps_text brightness create_pixel [] in
  (* let entity_primitives = draw_string (base_x*2) ((base_y + 14)*2) entity_text brightness create_pixel fps_primitives in *)
  draw_string (base_x*2) ((base_y + 14)*2) !cached_res_text brightness create_pixel fps_primitives

type input_state = {
  keys: KeyCodeSet.t;
  events: Event.t list;  (* Accumulated unified input events for the current frame. *)
  mouse: Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t
type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list


(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init width height title make_fullscreen =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title Sdl.Window.(if make_fullscreen then fullscreen else windowed) >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

let framebuffer_to_bigarray s buffer bitmap =
  let palette = Screen.palette s in
  Array.iteri (fun y row ->
    Array.iteri (fun x pixel ->
      bitmap.{x + (y * (Array.length row))} <- Palette.index_to_rgb palette pixel
    ) row
  ) (Framebuffer.to_array buffer)

let render_texture r texture s bitmap =
  let width, height = Screen.dimensions s in
  let scale = Screen.scale s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst = Sdl.Rect.create ~x:((ow - (width * scale)) / 2) ~y:((oh - (height * scale)) / 2) ~w:(width * scale) ~h:(height * scale) in
  Sdl.render_copy ~dst:dst r texture >|= fun () ->
  Sdl.render_present r

let create_overlay_pixel x y brightness acc =
    (Primitives.Pixel({ x = x; y = y }, brightness)) :: acc
  (* ----- *)

let cached_stats_primitives = ref []
let last_stats_update = ref 0.0

let last_f_key_state = ref false


(* Poll SDL events and build the unified event queue.
   Mouse events are handled by PlatformMouse.handle_event, which returns
   an updated mouse state along with a list of unified events. *)
let rec poll_all_events keys mouse acc =
  let e = Sdl.Event.create () in
  match Sdl.poll_event (Some e) with
  | true ->
      (match Sdl.Event.(enum (get e typ)) with
       | `Quit ->
           (true, keys, mouse, List.rev acc)
       | `Key_down ->
           let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
           poll_all_events (KeyCodeSet.add key keys) mouse (Event.KeyDown key :: acc)
       | `Key_up ->
           let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
           poll_all_events (KeyCodeSet.remove key keys) mouse (Event.KeyUp key :: acc)
       | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
           let (new_mouse, mouse_events) = PlatformMouse.handle_event e mouse in
           poll_all_events keys new_mouse (List.rev_append mouse_events acc)
       | `Drop_file ->
          let filepath = Sdl.Event.drop_file_file e in
          Sdl.Event.drop_file_free e;
          let updated_events = match filepath with
          | None -> acc
          | Some filepath -> Event.DropFile filepath :: acc
          in
          poll_all_events keys mouse updated_events
       | _ ->
           poll_all_events keys mouse acc)
  | false ->
      (false, keys, mouse, List.rev acc)

let run title boot tick s =
  let make_full =
    Array.to_list Sys.argv |> List.exists (fun a -> String.compare a "-f" = 0)
  in
  let s =
    match make_full with
    | false -> s
    | true ->
        let w, h = Screen.dimensions s in
        let p = Screen.palette s in
        let font = Screen.font s in
        Screen.create ~font w h 1 p
  in

  let width, height = Screen.dimensions s and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title make_full with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
      (match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height
              Sdl.Texture.access_streaming with
       | Error (`Msg e) ->
           Sdl.log "Texture error: %s" e;
           exit 1
       | Ok texture ->
           let bitmap =
             Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
           in
           let initial_buffer =
             match boot with
             | None -> Framebuffer.init (width, height) (fun _ _ -> 0)
             | Some bfunc -> bfunc s
           in
           let initial_input = {
             keys = KeyCodeSet.empty;
             events = [];
             mouse = Mouse.create scale;
           } in
           let rec loop t prev_buffer input last_t =
             let now = Sdl.get_ticks () in
             let diff = Int32.sub (Int32.of_int (1000 / 60)) (Int32.sub now last_t) in
             if Int32.compare diff Int32.zero > 0 then Sdl.delay diff;
             let exit, new_keys, new_mouse, unified_events =
               poll_all_events input.keys input.mouse []
             in
             let current_input = {
               keys = new_keys;
               events = unified_events;
               mouse = new_mouse;
             } in
             if exit then ()
             else begin

               calculate_fps ();

               let current_f_pressed = KeyCodeSet.mem stats_toggle_key input.keys in
               let toggle_stats = current_f_pressed && not !last_f_key_state in
               last_f_key_state := current_f_pressed;

               if toggle_stats then
                 show_stats := not !show_stats;


               Screenshot.save_screenshot current_input.events s prev_buffer;

               let updated_buffer = tick t s prev_buffer current_input in

               let updated_buffer =
                 if !show_stats then
                  let now = Unix.gettimeofday () in
                  if now -. !last_stats_update > 0.5 then (
                    (* Only update the visual stats every half second - using mli compatible function *)
                    cached_stats_primitives := render_stats create_overlay_pixel width height;
                    last_stats_update := now
                  );

                  Framebuffer.render updated_buffer !cached_stats_primitives;
                  Framebuffer.set_dirty updated_buffer;
                  updated_buffer
                else
                  updated_buffer
               in

               if (updated_buffer != prev_buffer)
                  || (Framebuffer.is_dirty updated_buffer)
                  || (Screen.is_dirty s)
               then (
                 framebuffer_to_bigarray s updated_buffer bitmap;
                 (match render_texture r texture s bitmap with
                  | Error (`Msg e) -> Sdl.log "Render error: %s" e
                  | Ok () -> ());
                 Framebuffer.clear_dirty updated_buffer;
                 Screen.clear_dirty s
               );
               (match render_texture r texture s bitmap with
                | Error (`Msg e) -> Sdl.log "Render error: %s" e
                | Ok () -> ());
               loop (t + 1) updated_buffer current_input now
             end
           in
           loop 0 initial_buffer initial_input Int32.zero;
           Sdl.destroy_texture texture;
           Sdl.destroy_renderer r;
           Sdl.destroy_window w;
           Sdl.quit ())

let run_functional title tick_f s =
  let wrap_tick t screen prev_framebuffer input =
    let primitives = tick_f t screen input in
    if primitives = [] then prev_framebuffer
    else
      let width, height = Screen.dimensions screen in
      let new_framebuffer = Framebuffer.init (width, height) (fun _ _ -> 0) in
      Framebuffer.render new_framebuffer primitives;
      new_framebuffer
  in
  run title None wrap_tick s

(* --- Utility functions for input handling --- *)

let is_key_pressed input key =
  KeyCodeSet.mem key input.keys

let was_key_just_pressed input key =
  List.exists (function
      | Event.KeyDown k when k = key -> true
      | _ -> false
    ) input.events

let was_key_just_released input key =
  List.exists (function
      | Event.KeyUp k when k = key -> true
      | _ -> false
    ) input.events

