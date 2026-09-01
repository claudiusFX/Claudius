open Js_of_ocaml
open Claudius

type t = {
  ctx : Dom_html.canvasRenderingContext2D Js.t;
  image_data : Dom_html.imageData Js.t;
  pixels : Dom_html.canvasPixelArray Js.t;
}

let v s title _make_fullscreen =
  let width, height = Screen.dimensions s and scale = Screen.scale s in
  let canvas =
    Js.Opt.get
      (Dom_html.CoerceTo.canvas (Dom_html.getElementById title))
      (fun () -> failwith "no canvas: ")
  in
  let ctx = canvas##getContext Dom_html._2d_ in
  canvas##.width := width * scale;
  canvas##.height := height * scale;
  let image_data =
    ctx##getImageData (Js.float 0.) (Js.float 0.)
      (Js.float (float_of_int width))
      (Js.float (float_of_int height))
  in
  let pixels = image_data##.data in
  (* Set all the alpha values to opaque *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let base = (x + (y * width)) * 4 in
      Dom_html.pixel_set pixels (base + 3) 255
    done
  done;
  Result.ok { ctx; image_data; pixels }

let shutdown _t = ()

let render t s fb =
  let w, h = Screen.dimensions s in
  let pal = Screen.palette s in
  let contents = Framebuffer.to_array fb in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let base = (x + (y * w)) * 4 in
      let row = contents.(y) in
      let p = row.(x) in
      let rgb = Int32.to_int (Palette.index_to_rgb pal p) in
      Dom_html.pixel_set t.pixels (base + 0) ((rgb lsl 16) land 0xff);
      Dom_html.pixel_set t.pixels (base + 1) ((rgb lsr 8) land 0xff);
      Dom_html.pixel_set t.pixels (base + 2) ((rgb lsr 0) land 0xff)
    done
  done;
  t.ctx##putImageData t.image_data (Js.float 0.) (Js.float 0.);
  Result.ok ()
