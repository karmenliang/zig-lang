module ProjectInterpreter
open ProjectParser
open Parser
open System

let PI = 3.14159265358979
let round (x:float) = int(Math.Round(x))

let toradians degrees =
    degrees * PI / 180.0

let aget s =
    let (c,t,p) = s
    let (x,y,a) = t
    a

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
    let dist' = round((float dist) * (cos a))
    dist'

let ycomp s dist =
    let (c,t,p) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (sin a))
    dist'

let xychange s x' y' =
    let (c,t,p) = s
    let (x,y,a) = t
    let t' = Turtle(x',y',a)
    let (wid,col,down) = p
    if down then
        let c' = Line(x,y,x',y',wid,col)::c
        (c',t',p)
    else (c,t',p)

let achange s a' =
    let (c,t,p) = s
    let (x,y,a) = t
    let t' = Turtle(x,y,a')
    (c,t',p)

// for debugging
let rec prettyprint e : string =
    match e with
    | Seq(e1,e2) -> prettyprint e1 + ";\n" + prettyprint e2
    | Ahead dist -> "ahead(" + dist.ToString() + ")"
    | Behind dist -> "behind(" + dist.ToString() + ")"
    | Clockwise degrees -> "clockwise(" + degrees.ToString() + ")"
    | Counterwise degrees -> "counterwise(" + degrees.ToString() + ")"
    | Press -> "press"
    | Lift -> "lift"
    | Pencolor(str) -> "pencolor " + (str)
    | Penwidth(i) -> "penwidth " + i.ToString()
    | Loop(i,e) -> "loop (" + i.ToString() + ")" + prettyprint e

// default assumptions: pen is down and angle is PI/2
let rec eval e s: State =
    match e with
    | Ahead dist ->
        let x' = (xget s)-(xcomp s dist)
        let y' = (yget s)-(ycomp s dist)
        xychange s x' y'
    | Behind dist ->
        let x' = (xget s)+(xcomp s dist)
        let y' = (yget s)+(ycomp s dist)
        xychange s x' y'
    | Clockwise degrees ->
        let radians = toradians (float degrees)
        let a' = (aget s) + radians
        achange s a'
    | Counterwise degrees ->
        let radians = toradians (float degrees)
        let a' = (aget s) - radians
        achange s a'
    | Press ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,true))
    | Lift ->
        let (c,t,p) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,false))
    | Seq(e1,e2) ->
        let s1 = eval e1 s
        eval e2 s1
    | Pencolor(color) ->
        let (c,t,p) = s
        let (w,_,d) = p
        (c,t,Pen(w,color,d))
    | Penwidth(i) ->
        let (c,t,p) = s
        let (_,color,d) = p
        (c,t,Pen(i,color,d))
    | Loop(i,e) ->
        if (i > 0) then 
            let s1 = eval e s
            eval (Loop(i-1,e)) s1
        else s