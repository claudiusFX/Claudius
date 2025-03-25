open Bdfparser

type t = 
  | Psf of Psf.t
  | Bdf of Bdf.t

type glyph = 
  | PsfGlyph of Psf.Glyph.t
  | BdfGlyph of Bdf.Glyph.t

let load_font (filename : string) : (t, string) result =
  match Filename.extension filename with
  | ".psf" -> 
    (match Psf.load_psf_font filename with
    | Ok psf -> Ok (Psf psf)
    | Error e -> Error e)
  | ".bdf" -> 
    (match Bdf.create filename with
    | Ok bdf -> Ok (Bdf bdf)
    | Error e -> Error e)
  | _ -> Error "Unknown font format"

let glyph_of_char (font : t) (u : Uchar.t) : glyph option =
  match font with
  | Psf psf -> 
    (match Psf.glyph_of_char psf u with
    | Some g -> Some (PsfGlyph g)
    | None -> None)
  | Bdf bdf -> 
    (match Bdf.glyph_of_char bdf u with
    | Some g -> Some (BdfGlyph g)
    | None -> None)

let glyph_count (font : t) : int =
  match font with
  | Psf psf -> Psf.glyph_count psf
  | Bdf bdf -> Bdf.glyph_count bdf

let glyph_dimensions (glyph : glyph) : (int * int * int * int) =
  match glyph with
  | PsfGlyph g -> Psf.Glyph.dimensions g
  | BdfGlyph b -> Bdf.Glyph.dimensions b

let glyph_bitmap (glyph : glyph) : bytes =
  match glyph with
  | PsfGlyph g -> Psf.Glyph.bitmap g
  | BdfGlyph b -> Bdf.Glyph.bitmap b