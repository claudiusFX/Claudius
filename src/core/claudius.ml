module Screen = Screen
module Framebuffer = Framebuffer
module Palette = Palette
module Picture = Picture
module Key = Key
module Event = Event
module Stats = Stats
module Mouse = Mouse
module Animation = Animation
module Primitives = Primitives
module Screenshot = Screenshot
module Font = Font
module Utils = Utils

type input_state = {
  keys : Key.KeyCodeSet.t;
  events : Event.t list;
      (* Accumulated unified input events for the current frame. *)
  mouse : Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t

type tick_func =
  int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type functional_tick_func = int -> Screen.t -> input_state -> Primitives.t list

(* --- Utility functions for input handling --- *)

let is_key_pressed input key = Key.KeyCodeSet.mem key input.keys

let was_key_just_pressed input key =
  List.exists
    (function Event.KeyDown k when k = key -> true | _ -> false)
    input.events

let was_key_just_released input key =
  List.exists
    (function Event.KeyUp k when k = key -> true | _ -> false)
    input.events
