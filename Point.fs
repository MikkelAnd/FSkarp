module Point

type Vector = Vector.Vector
type Point =
  | P of float * float * float
  override p.ToString() =
    match p with
      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

let makePoint x y z = P(x,y,z)
let getX (P(x,_,_)) = x
let getY (P(_,y,_)) = y
let getZ (P(_,_,z)) = z
let getCoordinates (P(x,y,z)) = (x,y,z)
let move (P(x1,y1,z1)) (V(x2,y2,z2)) = P(x1+x2, y1+y2, z1+z2) 
let distance (P(px,py,pz)) (P(qx,qy,qz)) = V(px-qx, py-qy, pz-qz)
let direction p q = Vector.normalise(distance p q)
let round (P(px,py,pz)) (d:int) = P(System.Math.Round(px,d), System.Math.Round(py,d), System.Math.Round(pz,d))

type Point with
  static member ( + ) (P(x1,y1,z1)) (V(x2,y2,z2)) = P(x1+x2, y1+y2, z1+z2)
  static member ( - ) (P(x1,y1,z1)) (P(x2,y2,z2)) = V(x1-x2, y1-y2, z1-z2)
