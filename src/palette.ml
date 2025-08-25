type t = int32 array

let generate_mono_palette (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int) : int32 ->
      let fi = float_of_int index and fsize = float_of_int size in
      let ch = fi /. fsize *. 255.0 in
      Int32.of_int
        ((int_of_float ch * 65536) + (int_of_float ch * 256) + int_of_float ch))

let generate_plasma_palette (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int) : int32 ->
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

      Int32.of_int
        ((int_of_float fred * 65536)
        + (int_of_float fgreen * 256)
        + int_of_float fblue))

let generate_linear_palette (color1 : int) (color2 : int) (size : int) : t =
  if size <= 0 then
    raise (Invalid_argument "Palette size must not be zero negative");
  let red1 = color1 / 65536 land 0xFF in
  let green1 = color1 / 256 land 0xFF in
  let blue1 = color1 land 0xFF in

  let red2 = color2 / 65536 land 0xFF in
  let green2 = color2 / 256 land 0xFF in
  let blue2 = color2 land 0xFF in
  Array.init size (fun index ->
      let ratio = float_of_int index /. float_of_int (size - 1) in

      let red = int_of_float (float red1 +. (float (red2 - red1) *. ratio)) in
      let green =
        int_of_float (float green1 +. (float (green2 - green1) *. ratio))
      in
      let blue =
        int_of_float (float blue1 +. (float (blue2 - blue1) *. ratio))
      in

      Int32.of_int (red * 65536 lor (green * 256) lor blue))

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
  Array.of_list
    [
      0x000000l;
      0x800000l;
      0x008000l;
      0x808000l;
      0x000080l;
      0x800080l;
      0x008080l;
      0xc0c0c0l;
      0x808080l;
      0xff0000l;
      0x00ff00l;
      0xffff00l;
      0x0000ffl;
      0xff00ffl;
      0x00ffffl;
      0xffffffl;
    ]

let generate_classic_vga_palette () : t =
  Array.of_list
    [
      0x000000l;
      0x0000AAl;
      0x00AA00l;
      0x00AAAAl;
      0xAA0000l;
      0xAA00AAl;
      0xAA5500l;
      0xAAAAAAl;
      0x555555l;
      0x5555FFl;
      0x55FF55l;
      0x55FFFFl;
      0xFF5555l;
      0xFF55FFl;
      0xFFFF55l;
      0xFFFFFFl;
    ]

let generate_sweetie16_palette () : t =
  (* This palette is by GrafxKid, found on Lospec:
     https://lospec.com/palette-list/sweetie-16
     Renamed here to match the original name: "Sweetie 16". *)
  Array.of_list
    [
      0x1a1c2cl;
      0x5d275dl;
      0xb13e53l;
      0xef7d57l;
      0xffcd75l;
      0xa7f070l;
      0x38b764l;
      0x257179l;
      0x29366fl;
      0x3b5dc9l;
      0x41a6f6l;
      0x73eff7l;
      0xf4f4f4l;
      0x94b0c2l;
      0x566c86l;
      0x333c57l;
    ]

let generate_mac_palette () : t =
  Array.of_list
    [
      0xffffffl;
      0xfcf400l;
      0xff6400l;
      0xdd0202l;
      0xf00285l;
      0x4600a5l;
      0x0000d5l;
      0x00aee9l;
      0x1ab90cl;
      0x006407l;
      0x572800l;
      0x917135l;
      0xc1c1c1l;
      0x818181l;
      0x3e3e3el;
      0x000000l;
    ]

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
  Array.map
    (fun (colorstr : string) : int32 ->
      Int32.of_int (int_of_string ("0x" ^ colorstr)))
    (Array.of_list raw)

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  if List.length strchunks > 0 then chunks_to_colors strchunks
  else raise (Invalid_argument "Palette size must not be zero or negative")

let of_list (rgb_list : int list) : t =
  if List.length rgb_list > 0 then
    Array.of_list (List.map Int32.of_int rgb_list)
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

let size (palette : t) : int = Array.length palette

let index_to_rgb (palette : t) (index : int) : int32 =
  let palsize = Array.length palette in
  let index = index mod palsize in
  palette.(if index >= 0 then index else index + palsize)

let to_list (palette : t) : int list =
  List.map Int32.to_int (Array.to_list palette)

let circle_palette (pal : t) (offset : int) : t =
  let size = Array.length pal in
  Array.init size (fun index ->
      (* Calculate new index ensuring it is positive *)
      let raw = index + offset in
      let new_index = if raw < 0 then (raw mod size) + size else raw mod size in
      pal.(new_index))

let updated_entry (pal : t) (index : int) (new_color : int * int * int) : t =
  if index < 0 || index >= Array.length pal then
    raise (Invalid_argument "Invalid palette index")
  else
    let r, g, b = new_color in
    let new_int = Int32.of_int (r * 65536 lor (g * 256) lor b) in
    let new_pal = Array.copy pal in
    new_pal.(index) <- new_int;
    new_pal

let concat (palettes : t list) : t =
  let total_len = List.fold_left (fun acc pal -> acc + Array.length pal) 0 palettes in
  let result = Array.make total_len 0l in
  let _ =
    List.fold_left (fun offset pal ->
      Array.iteri (fun i v -> result.(offset + i) <- v) pal;
      offset + Array.length pal
    ) 0 palettes
  in
  result
