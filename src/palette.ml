type t = int32 array


let generate_mono_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int): int32 ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = ((fi /. fsize) *. 255.0) in
    Int32.of_int (((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch))
  )

let generate_plasma_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int): int32 ->
    let fi = float_of_int index and fsize = float_of_int size in
    let fred = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fgreen = (cos ((fi +. (fsize /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fblue = (cos ((fi +. ((fsize *. 2.0) /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in

    Int32.of_int (((int_of_float fred) * 65536) + ((int_of_float fgreen) * 256) + (int_of_float fblue))
  )
  
let generate_linear_palette (color1 : int) (color2 : int) (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero negative");
  let red1   = (color1 / 65536) land 0xFF in
  let green1 = (color1 / 256)  land 0xFF in
  let blue1  = color1 land 0xFF in

  let red2   = (color2 / 65536) land 0xFF in
  let green2 = (color2 / 256)   land 0xFF in
  let blue2  = color2 land 0xFF in
  Array.init size (fun index ->
    let ratio = float_of_int index /. float_of_int (size - 1) in

    let red   = int_of_float (float red1   +. (float (red2 - red1)   *. ratio)) in
    let green = int_of_float (float green1 +. (float (green2 - green1) *. ratio)) in
    let blue  = int_of_float (float blue1  +. (float (blue2 - blue1)  *. ratio)) in

    Int32.of_int ((red * 65536) lor (green * 256) lor blue)
  )

let generate_vapourwave_palette (size : int) : t =
  let pastel_purple = 0x7f3b8f in  (* Pastel purple *)
  let pastel_cyan   = 0x80cfcf in  (* Pastel cyan *)
  generate_linear_palette pastel_purple pastel_cyan size 
     
let generate_microsoft_vga_palette () : t =
  (* This palette is by SZIEBERTH Ádám, found on Lospec:
     https://lospec.com/palette-list/microsoft-vga
     Renamed here to match the original name: "MICROSOFT VGA Palette". *)
   let colors = [
    0x000000; 0x800000; 0x008000; 0x808000;
    0x000080; 0x800080; 0x008080; 0xc0c0c0;
    0x808080; 0xff0000; 0x00ff00; 0xffff00;
    0x0000ff; 0xff00ff; 0x00ffff; 0xffffff;
  ] in
  of_list colors

let generate_classic_vga_palette () : t =
  let colors = [
    0x000000; 0x0000AA; 0x00AA00; 0x00AAAA;
    0xAA0000; 0xAA00AA; 0xAA5500; 0xAAAAAA;
    0x555555; 0x5555FF; 0x55FF55; 0x55FFFF;
    0xFF5555; 0xFF55FF; 0xFFFF55; 0xFFFFFF;
  ] in
   of_list colors

let generate_sweetie16_palette () : t =
  (* This palette is by GrafxKid, found on Lospec:
     https://lospec.com/palette-list/sweetie-16
     Renamed here to match the original name: "Sweetie 16". *)
  let colors = [
    0x1a1c2c; 0x5d275d; 0xb13e53; 0xef7d57; 
    0xffcd75; 0xa7f070; 0x38b764; 0x257179; 
    0x29366f; 0x3b5dc9; 0x41a6f6; 0x73eff7; 
    0xf4f4f4; 0x94b0c2; 0x566c86; 0x333c57;
  ] in
  of_list colors

let generate_mac_palette () : t =
  let colors = [
    0xffffff; 0xfcf400; 0xff6400; 0xdd0202;
    0xf00285; 0x4600a5; 0x0000d5; 0x00aee9;
    0x1ab90c; 0x006407; 0x572800; 0x917135;
    0xc1c1c1; 0x818181; 0x3e3e3e; 0x000000;
  ] in
  of_list colors

let string_to_chunks (x : string) (size : int) : string list =
  let rec loop sofar remainder =
    let length_left = String.length remainder in
    if length_left >= size then
      loop ((String.sub remainder 0 size) :: sofar) (String.sub remainder size (length_left - size))
    else if length_left == 0 then
      sofar
    else
      raise (Invalid_argument "String size not a multiple of 6 chars per colour")
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : t =
  Array.map (fun (colorstr : string): int32 -> Int32.of_int (int_of_string ("0x" ^ colorstr))) (Array.of_list raw)

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  if List.length strchunks > 0 then
    chunks_to_colors strchunks
  else
    raise (Invalid_argument "Palette size must not be zero or negative")

let size (palette : t) : int =
    Array.length palette

let index_to_rgb (palette : t) (index : int) : int32 =
  let palsize = Array.length palette in
  let index = index mod palsize in
  palette.(if index >= 0 then index else index + palsize)

let to_list (palette : t) : int list =
    List.map Int32.to_int (Array.to_list palette)

let of_list (rgb_list : int list) : t =
  if List.length rgb_list > 0 then
    Array.of_list (List.map Int32.of_int rgb_list)
  else
    raise (Invalid_argument "Palette size must not be zero or negative")
