module ProjectInterpreter
open ProjectParser
open Parser

// multiple assumptions: pen is down and angle is 0
let rec eval e s: State =
    match e with
    | Ahead dist ->
        let (c,t,p) = s
        let (x,y,a) = t
        let sdist = stringify(dist)
        let idist = sdist |> int
        let y' = y-idist
        let t' = Turtle(x,y',a) //functionalize this
        let c' = Line(x,y,x,y')::c
        (c',t',p)