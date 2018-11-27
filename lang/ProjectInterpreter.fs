module ProjectInterpreter
open ProjectParser
open Parser

let yget s =
    let (c,t,p) = s
    let (x,y,a) = t
    y

let ychange s y' =
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
        ychange s y'
    | Behind dist ->
        let y' = (yget s)+dist
        ychange s y'
    // | Seq(e1,e2) ->
    //     let s1 = eval e1 s
    //     let s2 = eval e2 s1
    //     s2
    // have not tested pendown or penup
    | Pendown ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,true))
    | Penup ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,false))