type t = { colors : int32 array; distinctive_pair : int * int }

let delta_e (luv1 : Hsluv.luv) (luv2 : Hsluv.luv) =
  sqrt
    (((luv1.l -. luv2.l) ** 2.0)
    +. ((luv1.u -. luv2.u) ** 2.0)
    +. ((luv1.v -. luv2.v) ** 2.0))

let find_most_distant_pair colors =
  (* To find the most visually distinct colours we project the
    colours from RGB into the LUV colour space:

    https://en.wikipedia.org/wiki/CIELUV

    Once in this colour space you can calculate the "delta E", the
    geometric distance between the colours, and that distance
    corresponds to visual distance.
  *)
  let luv_colors =
    Array.map
      (fun col ->
        let r = float_of_int (col / 65536 land 0xFF)
        and g = float_of_int (col / 256 land 0xFF)
        and b = float_of_int (col land 0xFF) in
        let rgb : Hsluv.rgb = { r; g; b } in
        Hsluv.conv_rgb_xyz rgb |> Hsluv.conv_xyz_luv)
      colors
  in
  let max_dist = ref 0. in
  let res = ref (0, 0) in
  let count = Array.length luv_colors in
  for outer = 0 to count - 1 do
    for inner = 0 to count - 1 do
      let luv_1 = luv_colors.(inner) and luv_2 = luv_colors.(outer) in
      let distance = delta_e luv_1 luv_2 in
      if distance > !max_dist then (
        max_dist := distance;
        res := (inner, outer))
    done
  done;

  (* In order to put some consistency on things, list the colours
  based on the most dark colour first. *)
  let index1, index2 = !res in
  let luv1 = luv_colors.(index1) and luv2 = luv_colors.(index2) in
  if luv1.l > luv2.l then (index2, index1) else (index1, index2)

let v colors =
  if Array.length colors == 0 then
    raise (Invalid_argument "Palette size must not be zero or negative");
  let distinctive_pair = find_most_distant_pair colors in
  let colors = Array.map Int32.of_int colors in
  { colors; distinctive_pair }

let generate_mono_palette (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero or negative");
  let colors =
    Array.init size (fun (index : int) : int ->
        let fi = float_of_int index and fsize = float_of_int size in
        let ch = fi /. fsize *. 255.0 in
        (int_of_float ch * 65536) + (int_of_float ch * 256) + int_of_float ch)
  in
  v colors

let generate_plasma_palette (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero or negative");
  let colors =
    Array.init size (fun (index : int) : int ->
        let fi = float_of_int index and fsize = float_of_int size in
        let fred = (cos (fi *. (2.0 *. Float.pi /. fsize)) *. 127.0) +. 128.0 in
        let fgreen =
          (cos ((fi +. (fsize /. 3.0)) *. (2.0 *. Float.pi /. fsize)) *. 127.0)
          +. 128.0
        in
        let fblue =
          cos ((fi +. (fsize *. 2.0 /. 3.0)) *. (2.0 *. Float.pi /. fsize))
          *. 127.0
          +. 128.0
        in
        (int_of_float fred * 65536)
        + (int_of_float fgreen * 256)
        + int_of_float fblue)
  in
  v colors

let generate_linear_palette (color1 : int) (color2 : int) (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero negative");
  let red1 = color1 / 65536 land 0xFF in
  let green1 = color1 / 256 land 0xFF in
  let blue1 = color1 land 0xFF in

  let red2 = color2 / 65536 land 0xFF in
  let green2 = color2 / 256 land 0xFF in
  let blue2 = color2 land 0xFF in
  let colors =
    Array.init size (fun index ->
        let ratio = float_of_int index /. float_of_int (size - 1) in

        let red = int_of_float (float red1 +. (float (red2 - red1) *. ratio)) in
        let green =
          int_of_float (float green1 +. (float (green2 - green1) *. ratio))
        in
        let blue =
          int_of_float (float blue1 +. (float (blue2 - blue1) *. ratio))
        in

        red * 65536 lor (green * 256) lor blue)
  in
  v colors

let generate_vapourwave_palette (size : int) : t =
  let pastel_purple = 0x7f3b8f in
  (* Pastel purple *)
  let pastel_cyan = 0x80cfcf in
  (* Pastel cyan *)
  generate_linear_palette pastel_purple pastel_cyan size

let generate_microsoft_vga_palette () : t =
  (* This palette is by SZIEBERTH Ádám, found on Lospec:
     https://lospec.com/palette-list/microsoft-vga
     Renamed here to match the original name: "MICROSOFT VGA Palette". *)
  let colors =
    Array.of_list
      [
        0x000000;
        0x800000;
        0x008000;
        0x808000;
        0x000080;
        0x800080;
        0x008080;
        0xc0c0c0;
        0x808080;
        0xff0000;
        0x00ff00;
        0xffff00;
        0x0000ff;
        0xff00ff;
        0x00ffff;
        0xffffff;
      ]
  in
  v colors

let generate_classic_vga_palette () : t =
  let colors =
    Array.of_list
      [
        0x000000;
        0x0000AA;
        0x00AA00;
        0x00AAAA;
        0xAA0000;
        0xAA00AA;
        0xAA5500;
        0xAAAAAA;
        0x555555;
        0x5555FF;
        0x55FF55;
        0x55FFFF;
        0xFF5555;
        0xFF55FF;
        0xFFFF55;
        0xFFFFFF;
      ]
  in
  v colors

let generate_sweetie16_palette () : t =
  (* This palette is by GrafxKid, found on Lospec:
     https://lospec.com/palette-list/sweetie-16
     Renamed here to match the original name: "Sweetie 16". *)
  let colors =
    Array.of_list
      [
        0x1a1c2c;
        0x5d275d;
        0xb13e53;
        0xef7d57;
        0xffcd75;
        0xa7f070;
        0x38b764;
        0x257179;
        0x29366f;
        0x3b5dc9;
        0x41a6f6;
        0x73eff7;
        0xf4f4f4;
        0x94b0c2;
        0x566c86;
        0x333c57;
      ]
  in
  v colors

let generate_mac_palette () : t =
  let colors =
    Array.of_list
      [
        0xffffff;
        0xfcf400;
        0xff6400;
        0xdd0202;
        0xf00285;
        0x4600a5;
        0x0000d5;
        0x00aee9;
        0x1ab90c;
        0x006407;
        0x572800;
        0x917135;
        0xc1c1c1;
        0x818181;
        0x3e3e3e;
        0x000000;
      ]
  in
  v colors

let string_to_chunks (x : string) (size : int) : string list =
  let rec loop sofar remainder =
    let length_left = String.length remainder in
    if length_left >= size then
      loop
        (String.sub remainder 0 size :: sofar)
        (String.sub remainder size (length_left - size))
    else if length_left == 0 then sofar
    else
      raise
        (Invalid_argument "String size not a multiple of 6 chars per colour")
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : t =
  let colors =
    Array.map
      (fun (colorstr : string) : int -> int_of_string ("0x" ^ colorstr))
      (Array.of_list raw)
  in
  v colors

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  if List.length strchunks > 0 then chunks_to_colors strchunks
  else raise (Invalid_argument "Palette size must not be zero or negative")

let of_list (rgb_list : int list) : t =
  if List.length rgb_list > 0 then v (Array.of_list rgb_list)
  else raise (Invalid_argument "Palette size must not be zero or negative")

let load_lospec_palette (s : string) : t =
  let lines = String.split_on_char '\n' s in
  let parse_hex line =
    let line = String.trim line in
    let hex =
      match (String.length line, line) with
      | 6, l -> l
      | 7, l when l.[0] = '#' -> String.sub l 1 6
      | _ ->
          raise
            (Invalid_argument
               "Palette size must not be zero or invalid HEX values")
    in
    match int_of_string_opt ("0x" ^ hex) with
    | Some n -> n
    | None ->
        raise (Invalid_argument ("Failed to parse hex color: \"" ^ line ^ "\""))
  in
  let color_list = List.map parse_hex lines in
  if color_list = [] then
    raise
      (Invalid_argument "Palette size must not be zero or invalid HEX values");
  of_list color_list

let size (palette : t) : int = Array.length palette.colors

let index_to_rgb (palette : t) (index : int) : int32 =
  let palsize = Array.length palette.colors in
  let index = index mod palsize in
  palette.colors.(if index >= 0 then index else index + palsize)

let to_list (palette : t) : int list =
  List.map Int32.to_int (Array.to_list palette.colors)

let circle_palette (pal : t) (offset : int) : t =
  let size = Array.length pal.colors in
  let colors =
    Array.init size (fun index ->
        (* Calculate new index ensuring it is positive *)
        let raw = index + offset in
        let new_index =
          if raw < 0 then (raw mod size) + size else raw mod size
        in
        pal.colors.(new_index))
  in
  { pal with colors }

let updated_entry (pal : t) (index : int) (new_color : int * int * int) : t =
  let palsize = Array.length pal.colors in
  if index < 0 || index >= palsize then
    raise (Invalid_argument "Invalid palette index")
  else
    let r, g, b = new_color in
    let new_int = r * 65536 lor (g * 256) lor b in
    let new_pal = Array.init palsize (fun i -> Int32.to_int pal.colors.(i)) in
    new_pal.(index) <- new_int;
    v new_pal

let concat (palettes : t list) : t =
  let total_len =
    List.fold_left (fun acc pal -> acc + Array.length pal.colors) 0 palettes
  in
  let result = Array.make total_len 0 in
  let _ =
    List.fold_left
      (fun offset pal ->
        Array.iteri
          (fun i v -> result.(offset + i) <- Int32.to_int v)
          pal.colors;
        offset + Array.length pal.colors)
      0 palettes
  in
  v result

let distinctive_pair t = t.distinctive_pair
