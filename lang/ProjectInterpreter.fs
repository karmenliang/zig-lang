module ProjectInterpreter
open ProjectParser
open Parser
open System

let PI = 3.14159265358979
let round (x:float) = int(Math.Round(x))

let toradians degrees =
    degrees * PI / 180.0

let aget s =
    let (_,t,_,_) = s
    let (x,y,a) = t
    a

let xget s =
    let (_,t,_,_) = s
    let (x,y,a) = t
    x

let yget s =
    let (_,t,_,_) = s
    let (x,y,a) = t
    y

let xcomp s dist =
    let (_,t,_,_) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (cos a))
    dist'

let ycomp s dist =
    let (_,t,_,_) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (sin a))
    dist'

let xychange s x' y' =
    let (c,t,p,ctx) = s
    let (x,y,a) = t
    let t' = Turtle(x',y',a)
    let (wid,col,down) = p
    if down then
        let c' = Line(x,y,x',y',wid,col)::c
        (c',t',p,ctx)
    else (c,t',p,ctx)

let achange s a' =
    let (c,t,p,ctx) = s
    let (x,y,a) = t
    let t' = Turtle(x,y,a')
    (c,t',p,ctx)

// for debugging
let rec prettyprint e : string =
    match e with
    | StringVal s -> "\"" + s + "\""
    | NumVal n -> n.ToString()
    | Seq(e1,e2) -> prettyprint e1 + ";\n" + prettyprint e2
    | Ahead dist -> "ahead(" + dist.ToString() + ")"
    | AheadVar var -> "ahead(" + var + ")"
    | Behind dist -> "behind(" + dist.ToString() + ")"
    | Clockwise degrees -> "clockwise(" + degrees.ToString() + ")"
    | Counterwise degrees -> "counterwise(" + degrees.ToString() + ")"
    | Press -> "press"
    | Lift -> "lift"
    | Pencolor(str) -> "pencolor " + (str)
    | Penwidth(i) -> "penwidth " + i.ToString()
    | Loop(i,e) -> "loop (" + i.ToString() + ")" + prettyprint e
    | Assign(v,e) -> v + " = " + e.ToString()

// for debugging
let valueprint v : string =
    match v with
    | ValueString s -> s
    | ValueNum n -> n.ToString()
    //| ValueExpr e -> prettyprint e

// let var v s : int =
//     match v with
//     | StringVal sv -> 
//         let (c,t,p,ctx) = s
//         let n = Map.find sv ctx

//     | NumVal n -> n

// default assumptions: pen is down and angle is PI/2
let rec eval e s: State =
    match e with
    | StringVal sv -> s
    | NumVal n -> s
    | Ahead dist ->
        let x' = (xget s)-(xcomp s dist)
        let y' = (yget s)-(ycomp s dist)
        xychange s x' y'
    | AheadVar var ->
        let (c,t,p,ctx) = s
        let ni = match ctx.[var] with
        | ValueNum n -> n
        | _ -> failwith ""
        let x' = (xget s)-(xcomp s ni)
        let y' = (yget s)-(ycomp s ni)
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
        let (c,t,p,ctx) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,true),ctx)
    | Lift ->
        let (c,t,p,ctx) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,false),ctx)
    | Seq(e1,e2) ->
        let s1 = eval e1 s
        eval e2 s1
    | Pencolor(color) ->
        let (c,t,p,ctx) = s
        let (w,_,d) = p
        (c,t,Pen(w,color,d),ctx)
    | Penwidth(i) ->
        let (c,t,p,ctx) = s
        let (_,color,d) = p
        (c,t,Pen(i,color,d),ctx)
    | Loop(i,e) ->
        if (i > 0) then 
            let s1 = eval e s
            eval (Loop(i-1,e)) s1
        else s
    // globally dynamically scoped
    | Assign(str,e) ->
        let s1 = eval e s 
        let (c,t,p,ctx) = s1
        match e with
        | StringVal sv ->
            let ctx1 = Map.add str (ValueString sv) ctx
            printfn "assign stringval: %A" ctx1
            (c,t,p,ctx1)
        | NumVal n ->
            let ctx1 = Map.add str (ValueNum n) ctx
            printfn "assign numval: %A" ctx1
            (c,t,p,ctx1)
        // BUG: does not properly assign Exprs
        | _ ->
        //    let ctx1 = Map.add str (ValueExpr e) ctx
        //    printf "assign exprval: %A" ctx1
            failwith "can never happen" // Dan
            //(c,t,p,ctx1)
