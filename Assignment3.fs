module Test
open System.Security.Cryptography.X509Certificates
open System.Security.Cryptography.X509Certificates
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

// 3.1
let rec inOrder a = match a with 
    | Leaf -> []
    | Node(x,tl,tr) -> (inOrder tl) @ [x] @ (inOrder tr);;

// 3.2
let rec mapInOrder f a =
    match a with 
        | Node(x,tl,tr) -> Node(f x, mapInOrder f tl, mapInOrder f tr)
        | Leaf -> Leaf

// 3.3
let rec foldInOrder f e t =
    match t with
    | Leaf -> e
    | Node(x,tl,tr) ->
        let ex = f x e
        let er = foldInOrder f ex tr
        foldInOrder f er tl;;

//3.4 + 3.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm          // if-then
    | RU of stm * bExp          // repeat until 

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(a1,a2) -> A a1 s = A a2 s
    | Lt(a1,a2) -> A a1 s < A a2 s
    | Neg b1 -> not (B b1 s)
    | Con(b1,b2) -> B b1 s && B b2 s

let update x v s = Map.add x v s

let rec I stm s =
    match stm with
    | Ass(x,a) -> update x (A a s) s
    | Skip -> s
    | Seq(stm1,stm2) -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2) ->
        if (B b s) 
        then I stm1 s 
        else I stm2 s
    | While(b, stmInner) -> 
        if (B b s) 
        then I stm (I stmInner s) 
        else s
    | IT(b,stm) -> 
        if (B b s)
        then I stm s
        else s
    | RU(stm,b) -> 
        let s' = I stm s
        if not (B b s') 
        then I (RU(stm,b)) s'
        else s'
       

// Example 1
let example1 a1 a2 = A (Add(N a1, N a2)) Map.empty

// Example 2
let example2 a1 a2 = B (Eq(N a1, N a2)) Map.empty

// Example 3
let example3 b stm = I (IT(b, stm)) Map.empty

// Example 4
let example4 b = B (Neg(b)) Map.empty 

// Example 5
let example5 a1 a2 a3 a4 = B (Lt((Mul(N a1, N a2)), (Mul(N a3, N a4)))) Map.empty

// 3.6
(*
    
*)

// 3.7 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;

let rec fexprToString = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1,fe2) -> "(" + (fexprToString fe1) + ")" + " + " + "(" + (fexprToString fe2) + ")"
    | Sub(fe1,fe2) -> "(" + (fexprToString fe1) + ")" + " - " + "(" + (fexprToString fe2) + ")"
    | Mul(fe1,fe2) -> "(" + (fexprToString fe1) + ")" + " * " + "(" + (fexprToString fe2) + ")"
    | Div(fe1,fe2) -> "(" + (fexprToString fe1) + ")" + " / " + "(" + (fexprToString fe2) + ")"
    | Sin fe -> "sin(" + (fexprToString fe) + ")"
    | Cos fe -> "cos(" + (fexprToString fe) + ")"
    | Log fe -> "log(" + (fexprToString fe) + ")"
    | Exp fe -> "exp(" + (fexprToString fe) + ")";;

// 3.8 (HR 6.8)

type Stack = S of float list

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float

let intpInstr (s:float list) (i:Instruction) =
    match i with
    | PUSH x -> x::s
    | ADD ->
        match s with
        | x1::x2::s -> (x1 + x2)::s
        | _ -> failwith "not enough elements on stack"
    | SUB -> 
        match s with
        | x1::x2::s -> (x1 - x2)::s
        | _ -> failwith "not enough elements on stack"
    | MULT ->
        match s with
        | x1::x2::s -> (x1 * x2)::s
        | _ -> failwith "not enough elements on stack"
    | DIV ->
        match s with
        | x1::x2::s -> (x1 / x2)::s
        | _ -> failwith "not enough elements on stack"
    | SIN ->
        match s with
        | x::s -> (System.Math.Sin(x))::s
        | _ -> failwith "not enough elements on stack"
    | COS ->
        match s with
        | x::s -> (System.Math.Cos(x))::s
        | _ -> failwith "not enough elements on stack"
    | LOG ->
        match s with
        | x::s -> (System.Math.Log(x))::s
        | _ -> failwith "not enough elements on stack"
    | EXP ->
        match s with
        | x::s -> (System.Math.Exp(x))::s
        | _ -> failwith "not enough elements on stack"
   
let rec runProgram il s =
    match il with
    | [] -> s
    | Instruction::rest ->
        runProgram rest (intpInstr s Instruction)

let intpProg (il:Instruction list) : float = 
    match (runProgram il []) with
    | result::_ -> result
    | _ -> failwith "no more results left in stack"

let rec trans ((fe:Fexpr), (x:float)) = 
    match fe with
    | Const a -> [PUSH a]
    | X -> [PUSH x]
    | Add(a,b) -> 
        let resa = trans (a,x)
        let resb = trans (b,x)
        List.concat [resa;resb;[ADD]]
    | Sub(a,b) ->
        let resa = trans (a,x)
        let resb = trans (b,x)
        List.concat [resa;resb;[SUB]]
    | Mul(a,b) ->
        let resa = trans (a,x)
        let resb = trans (b,x)
        List.concat [resa;resb;[MULT]]
    | Div(a,b) ->
        let resa = trans (a,x)
        let resb = trans (b,x)
        List.concat [resa;resb;[DIV]]
    | Sin(a) ->
        let resa = trans (a,x)
        List.concat [resa;[SIN]]
    | Cos(a) ->
        let resa = trans (a,x)
        List.concat [resa;[COS]]
    | Log(a) ->
        let resa = trans (a,x)
        List.concat [resa;[LOG]]
    | Exp(a) ->
        let resa = trans (a,x)
        List.concat [resa;[EXP]]

// 3.9 (HR 7.2)
module ComplexNumber =
    type ComplexNumber =
        | C of float * float

    type ComplexNumber with
        static member (-) (C(a,b)) (C(c,d)) = C(a-c,b-d)
        static member (+) (C(a,b)) (C(c,d)) = C(a+c,b+d)
        static member (*) (C(a,b)) (C(c,d)) = C((a*c)-(b*d),(b*c)+(a*d))
        static member (/) (C(a,b)) (C(c,d)) = ((a*c + b*d)/(c*c+d*d)) + ((b*c-a*d)/(c*c+d*d))

    let makeComplex (a,b) = (C(a,b))
    let getValue (C(a,b)) = (a, b)
