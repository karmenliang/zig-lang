module ProjectInterpreter
open ProjectParser
open Parser

let yget s =
    let (c,t,p) = s
    let (x,y,a) = t
    y

let newState s y' =
    let (c,t,p) = s
    let (x,y,a) = t
    let t' = Turtle(x,y',a)
    let c' = Line(x,y,x,y')::c
    (c',t',p)

// multiple assumptions: pen is down and angle is 0
let rec eval e s: State =
    match e with
    | Ahead dist ->
        // let (c,t,p) = s
        // let (x,y,a) = t
        let y' = (yget s)-dist
        // let t' = Turtle(x,y',a) //functionalize this
        // let c' = Line(x,y,x,y')::c
        // (c',t',p)
        newState s y'
    | Behind dist ->
        let y' = (yget s)+dist
        newState s y'