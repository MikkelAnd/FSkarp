module Colour

exception ColourException
type Colour =
  | RGB of float * float * float
  override rgb.ToString() =
    match rgb with
      RGB(r,g,b) -> "["+r.ToString()+","+g.ToString()+","+b.ToString()+"]"

let mkColour r g b =
  if r < 0.0
    then raise ColourException
  elif g < 0.0
    then raise ColourException
  elif b < 0.0
    then raise ColourException
  else RGB(r,g,b)
let getR (RGB(r,_,_)) = r
let getG (RGB(_,g,_)) = g
let getB (RGB(_,_,b)) = b
let scale (RGB(r,g,b)) s =
  if r < 0.0
    then raise ColourException
  elif g < 0.0
    then raise ColourException
  elif b < 0.0
    then raise ColourException
  else RGB(r*s, g*s, b*s)
let merge w (RGB(r1,g1,b1)) (RGB(r2,g2,b2)) = 
  if w < 0.0 && w > 1.0
  then raise ColourException
  else RGB(w*r1+(1.0 - w)*r2, w*g1+(1.0 - w)*g2, w*b1+(1.0 - w)*b2)

let toColor (RGB(r,g,b)) = 
  System.Drawing.Color.FromArgb
    (min (int (sqrt r*255.0)) 255,
    min (int (sqrt g*255.0)) 255,
    min (int (sqrt b*255.0)) 255)

let fromColor (c:System.Drawing.Color) =
  mkColour (System.Math.Pow (float c.R / 255.0, 2.0))
    (System.Math.Pow (float c.G / 255.0, 2.0))
    (System.Math.Pow (float c.B / 255.0, 2.0))

type Colour with
  static member ( + ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = RGB(r1+r2, g1+g2, b1+b2)
  static member ( * ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = RGB(r1*r2, g1*g2, b1*b2)
  static member ( * ) (RGB(r,g,b), (s)) = RGB(r*s, g*s, b*s)