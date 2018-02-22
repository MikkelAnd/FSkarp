// Exercise 5.1
module Vector =
    type Vector =
        | V of float * float * float
    
    type Vector with
        static member (~-) (V(x,y,z)) = V(-x,-y,-z)
        static member (+) (V(x1,y1,z1),V(x2,y2,z2))= V((x1+x2),(y1+y2),(z1+z2)) 
        static member (~-) (V(x1,y1,z1),V(x2,y2,z2)) = V((x1-x2),(y1-y2),(z1-z2))
        static member (*) (V(x,y,z),(s)) = V(x*s,y*s,z*s)
        static member (*) (V(x1,y1,z1),V(x2,y2,z2)) = V((x1*x2),(y1*y2),(z1*z2))
        static member (%) (V(x1,y1,z1),V(x2,y2,z2)) = V((y2*z1)-(z2*y1),(z2*x1)-(x2*z1),(x2*y1)-(y2*x1))
        static member (/) (V(x,y,z),(s)) = V(x*s,y*s,z*s)
    
    let makeVector (x,y,z) = V(x,y,z)
    let getX (V(x,y,z)) = x
    let getY (V(x,y,z)) = y
    let getZ (V(x,y,z)) = z
    let getCoordinate (V(x,y,z)) = (x,y,z)
    let multiplyScalar (s) (V(x,y,z)) = V(x*s,y*s,z*s)
    let magnitude (V(x,y,z)) = System.Math.Sqrt(x*x + y*y + z*z)
    let dotProduct (V(x1,y1,z1),V(x2,y2,z2)) = V((x1*x2),(y1*y2),(z1*z2))
    let crossProduct (V(x1,y1,z1),V(x2,y2,z2)) = V((y2*z1)-(z2*y1),(z2*x1)-(x2*z1),(x2*y1)-(y2*x1))
    let normalize (V(x,y,z)) = V(x/magnitude(V(x,y,z)), y/magnitude(V(x,y,z)), z/magnitude(V(x,y,z)))
    let round (V(x,y,z)) (i:int) = V(System.Math.Round(x,i), System.Math.Round(y,i), System.Math.Round(z,i))