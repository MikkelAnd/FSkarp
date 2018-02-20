// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow(x,n)

// Exercise 1.3
let g n = n + 4

// Exercise 1.4
let h (x, y) = System.Math.Sqrt(x*x + y*y)

// Exercise 1.5
let rec f = function
| 0 -> 1
| n -> n + f(n-1)

    // f 4;; val it : int = 11

// Exercise 1.6
let rec fibno = function
| 0 -> 0
| 1 -> 1
| n -> fibno(n-1) + fibno(n-2)

    // fibno 4;; val it : int = 3

// Exercise 1.7 
let rec sum = function 
| (m,0) -> m
| (m,n) -> (m + n) + sum(m, n-1)


// Exercise 1.8
(*
    (System.Math.PI, fact -1) ->        (float, int)

    fact(fact 4) ->                     int

    power(System.Math.PI, fact 2) ->    float
       
    (power, fact) ->                    ((float * int) -> float, int -> int)
*)

// Exercise 1.9
(*
           
                [   a -> 5   ]
        env1=   [   f a -> "a function that adds 1 to any value passed in a"  ]
                [   f g b -> "a function that uses another function whics adds 1 to b and then adds the predefined a value to the result"   ]

        f 3;;
        val it : int = 4

        g 3;;
        val it : int = 9

*)

// Exercise 1.10

let dub x:string = x + x

// Exercise 1.11

let rec dubn (s:string) n =
    match n with
    |   0 -> ""
    |   _ -> s + dubn s (n-1)

// Exercise 1.12

let timediff (hh1, mm1) (hh2, mm2) =  ((hh2)*60 + mm2) - ((hh1)*60 + mm1) 

// Exercise 1.13

let minutes = timediff (00, 00)

// Exercise 1.14 

let rec stringconcat = function
|   (s:string,1) -> s
|   (s:string,n) -> s + stringconcat(s,n-1)

// Exercise 1.15

let rec pascal = function
|   (n,0) -> 1
|   (n,k) when n = k -> 1
|   (n,k) -> pascal(n-1,k-1) + pascal(n-1, k)

// Exercise 1.16

    // 1.   (int * int) -> int
    // 2.   terminates when x = 0 and returns y
    // 3.   f(2, 3) -> (1, 6) -> (0, 6) -> 6
    // 4.   x! * y

// Exercise 1.17

    // 1. (bool * int) -> int
    // 2. it will potentially evaluate to stackoverflow
    // 3. it will always give 0 because of lazy evaluation


// Exercise 1.18
let curry f x y = f(x,y)
let uncurry f (x,y) = f x y