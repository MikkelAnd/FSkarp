// Exercise 5.1
module Vector =
    [<Sealed>]
    type Vector = 
        static member (~-) : Vector -> Vector
        static member (+) : Vector * Vector -> Vector
        static member (~-) : Vector * Vector -> Vector
        static member (*) : float * Vector -> Vector
        static member (*) : Vector * Vector -> float
        static member (%) : Vector * Vector -> Vector
        static member (/) : Vector * float -> Vector

    val makeVector : float -> float -> float -> Vector
    val getX : Vector -> float
    val getY : Vector -> float
    val getZ : Vector -> float
    val getCoordinate : Vector -> float * float * float
    val multiplyScalar : float -> Vector -> Vector
    val magnitude : Vector -> float 
    val dotProduct : Vector -> Vector -> float
    val crossProduct : Vector -> Vector -> Vector
    val normalize : Vector -> Vector
    val round : Vector -> int -> Vector


