(* Calculate and update the FPS counter *)
val calculate_fps : unit -> unit

(* Get the current FPS value *)
val fps_counter : int ref

(* Key to toggle stats display *)
val stats_toggle_key : Key.t

(* Render stats with a create_pixel function that builds primitives *)
val render_stats : 
  (int -> int -> int -> 'a list -> 'a list) ->  (* create_pixel function *)
  int ->                                       (* entity_count *)
  int ->                                       (* width *) 
  int ->                                       (* height *)
  'a list                                      (* returns a list of primitives *)