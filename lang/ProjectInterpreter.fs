module ProjectInterpreter
open ProjectParser
open Parser
open System

let PI = 3.14159265358979
let round (x:float) = int(Math.Round(x))

let to_radians degrees =
    degrees * PI / 180.0

let aget s =
    let (c,t,p) = s
    let (x,y,a) = t
    (float a)

let xget s =
    let (c,t,p) = s
    let (x,y,a) = t
    x

let yget s =
    let (c,t,p) = s
    let (x,y,a) = t
    y

let xcomp s dist =
    let (c,t,p) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (cos (float a)))
    dist'

let ycomp s dist =
    let (c,t,p) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (sin (float a)))
    dist'

let xychange s x' y' =
    let (c,t,p) = s
    let (x,y,a) = t
    let t' = Turtle(x',y',a)
    let c' = Line(x,y,x',y')::c
    (c',t',p)

let achange s a' =
    let (c,t,p) = s
    let (x,y,a) = t
    let t' = Turtle(x,y,a')
    (c,t',p)

// default assumptions: pen is down and angle is 0
let rec eval e s: State =
    match e with
    | Seq(e1,e2) ->
        let s1 = eval e1 s
        eval e2 s1
    | Ahead dist ->
        let x' = (xget s)-(xcomp s dist)
        let y' = (yget s)-(ycomp s dist)
        xychange s x' y'
    | Behind dist ->
        let x' = (xget s)+(xcomp s dist)
        let y' = (yget s)+(ycomp s dist)
        xychange s x' y'
    | Clockwise degrees ->
        let radians = to_radians (float degrees)
        let a' = (aget s) + radians
        achange s a'
    // have not tested pendown or penup
    | Pendown ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,true))
    | Penup ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,false))