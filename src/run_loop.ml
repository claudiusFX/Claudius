open Tsdl

let rec event_loop () =
  match Sdl.poll_event () with
  | Some event ->
      (match Sdl.Event.(enum (get event typ)) with
      | `Quit -> ()
      | _ -> event_loop ())
  | None -> event_loop ()

let run () =
  if Sdl.init Sdl.Init.video <> 0 then
    failwith (Printf.sprintf "SDL_Init Error: %s" (Sdl.get_error ()))
  else begin
    event_loop ();
    Sdl.quit ()
  end


