module ProjectInterpreter
open ProjectParser
open Parser
open System

let debug = false
let PI = 3.14159265358979
let round (x:float) = int(Math.Round(x))

let toradians degrees =
    degrees * PI / 180.0

let aget s =
    let (_,t,_,_,_) = s
    let (x,y,a) = t
    a

let xget s =
    let (_,t,_,_,_) = s
    let (x,y,a) = t
    x

let yget s =
    let (_,t,_,_,_) = s
    let (x,y,a) = t
    y

let xcomp s dist =
    let (_,t,_,_,_) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (cos a))
    dist'

let ycomp s dist =
    let (_,t,_,_,_) = s
    let (x,y,a) = t
    let dist' = round((float dist) * (sin a))
    dist'

let xychange s x' y' =
    let (c,t,p,ctx,b) = s
    let (x,y,a) = t
    let t' = Turtle(x',y',a)
    let (wid,rgb,down) = p
    if down then
        let c' = Line(x,y,x',y',wid,rgb)::c
        (c',t',p,ctx,b)
    else (c,t',p,ctx,b)

let achange s a' =
    let (c,t,p,ctx,b) = s
    let (x,y,a) = t
    let t' = Turtle(x,y,a')
    (c,t',p,ctx,b)

// for debugging
let rec prettyprint e : string =
    if debug then 
        match e with
        | StringExpr s -> "\"" + s + "\""
        | NumExpr n -> n.ToString()
        | Seq(e1,e2) -> prettyprint e1 + ";\n" + prettyprint e2
        | Ahead dist -> "ahead(" + dist.ToString() + ")"
        | Behind dist -> "behind(" + dist.ToString() + ")"
        | Clockwise degrees -> "clockwise(" + degrees.ToString() + ")"
        | Counterwise degrees -> "counterwise(" + degrees.ToString() + ")"
        | Press -> "press"
        | Lift -> "lift"
        | Pencolor(str) -> "pencolor " + (str)
        | Penred(x) -> "penred " + (x.ToString())
        | Pengreen(x) -> "pengreen " + (x.ToString())
        | Penblue(x) -> "penblue " + (x.ToString())
        | Penwidth(i) -> "penwidth " + i.ToString()
        | Loop(i,e) -> "loop (" + i.ToString() + ")" + prettyprint e
        | Assign(v,e) -> v + " = " + e.ToString()
        | UnaryIncrement(str) -> str + "++"
        | Increment(str,e) -> str + " += " + e.ToString()
        | UnaryDecrement(str) -> str + "--"
        | Decrement(str,e) -> str + " -= " + e.ToString()
        | GoHome -> "home"
        | SetHome(a,b) -> "sethome " + a.ToString() + ", " + b.ToString()
        | SetDimensions(a,b) -> "setdimensions " + a.ToString() + ", " + b.ToString()
        | PenRGB(a,b,c) -> "penrgb " + a.ToString() + ", " + b.ToString()+ ", " + c.ToString()
    else ""

// for debugging
let valueprint v : string =
    if debug then
        match v with
        | ValueString s -> s
        | ValueNum n -> n.ToString()
    else ""

let getnumval (v : Expr) (s : State) : int =
    match v with
    // if arg is a variable, extract value from Context Map
    | StringExpr sv -> 
        let (c,t,p,ctx,b) = s
        let n = match (Map.find sv ctx) with
                | ValueNum num -> num
                | ValueString vs -> failwith ("Variable" + sv + " is not a number")
        n
    | NumExpr n -> n
    // any other Expr is incorrect (not a variable or a number)
    | _ -> failwith "Argument must be a variable or number"

// default: pen is down and angle is PI/2
let rec eval e s: State =
    match e with
    | StringExpr sv -> s
    | NumExpr n -> s
    | Ahead arg ->
        let dist = getnumval arg s
        let x' = (xget s)-(xcomp s dist)
        let y' = (yget s)-(ycomp s dist)
        xychange s x' y'
    | Behind arg ->
        let dist = getnumval arg s
        let x' = (xget s)+(xcomp s dist)
        let y' = (yget s)+(ycomp s dist)
        xychange s x' y'
    | Clockwise arg ->
        let degs = getnumval arg s
        let radians = toradians (float degs)
        let a' = (aget s) + radians
        achange s a'
    | Counterwise arg ->
        let degs = getnumval arg s
        let radians = toradians (float degs)
        let a' = (aget s) - radians
        achange s a'
    | Press ->
        let (c,t,p,ctx,b) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,true),ctx,b)
    | Lift ->
        let (c,t,p,ctx,b) = s
        let (w,color,_) = p
        (c,t,Pen(w,color,false),ctx,b)
    | Seq(e1,e2) ->
        let s1 = eval e1 s
        eval e2 s1
    | Pencolor(color) -> // CHANGE THIS
        let (c,t,p,ctx,b) = s
        let (w,_,d) = p
        match color with
        | "black" -> (c,t,Pen(w,(0,0,0),d),ctx,b)
        | "red" -> (c,t,Pen(w,(255,0,0),d),ctx,b)
        | "green" -> (c,t,Pen(w,(0,255,0),d),ctx,b)
        | "blue" -> (c,t,Pen(w,(0,0,255),d),ctx,b)
        | _ -> (c,t,Pen(w,(0,0,0),d),ctx,b)
    | Penred arg ->
        let comp = getnumval arg s
        let (c,t,p,ctx,bound) = s
        let (w,rgb,d) = p
        let (_,g,b) = rgb
        let rgb' = (comp,g,b)
        (c,t,Pen(w,rgb',d),ctx,bound)
    | Pengreen arg ->
        let comp = getnumval arg s
        let (c,t,p,ctx,bound) = s
        let (w,rgb,d) = p
        let (r,g,b) = rgb
        let rgb' = (r,comp,b)
        (c,t,Pen(w,rgb',d),ctx,bound)
    | Penblue arg ->
        let comp = getnumval arg s
        let (c,t,p,ctx,bound) = s
        let (w,rgb,d) = p
        let (r,g,b) = rgb
        let rgb' = (r,g,comp)
        (c,t,Pen(w,rgb',d),ctx,bound)
    | Penwidth arg ->
        let width = getnumval arg s
        if width < 1 then failwith "Pen width must be positive"
        let (c,t,p,ctx,bound) = s
        let (_,color,d) = p
        (c,t,Pen(width,color,d),ctx,bound)
    | Loop(arg,e) ->
        let i = getnumval arg s
        if (i > 0) then
            let s1 = eval e s
            eval (Loop(NumExpr (i-1),e)) s1
        else s
    // globally dynamically scoped
    | Assign(str,e) ->
        let s1 = eval e s 
        let (c,t,p,ctx,bound) = s1
        match e with
        | StringExpr sv ->
            let ctx1 = Map.add str (ValueString sv) ctx
            printfn "assign stringval: %A" ctx1
            (c,t,p,ctx1,bound)
        | NumExpr n ->
            let ctx1 = Map.add str (ValueNum n) ctx
            printfn "assign numval: %A" ctx1
            (c,t,p,ctx1,bound)
        // BUG: does not properly assign Exprs
        | _ ->
        //    let ctx1 = Map.add str (ValueExpr e) ctx
        //    printf "assign exprval: %A" ctx1
            failwith "can never happen" // Dan
            //(c,t,p,ctx1)
    | UnaryIncrement(str) ->
        let (c,t,p,ctx,bound) = s
        let ni = match ctx.[str] with
                 | ValueNum n -> n
                 | _ -> failwith ""
        let ctx1 = Map.add str (ValueNum (ni + 1)) ctx
        (c,t,p,ctx1,bound)
    | Increment(str,e) ->
        let (c,t,p,ctx,bound) = s
        let ni = match ctx.[str] with
                 | ValueNum n -> n
                 | _ -> failwith ""
        let ctx1 = Map.add str (ValueNum (ni + e)) ctx
        (c,t,p,ctx1,bound)
    | UnaryDecrement(str) ->
        let (c,t,p,ctx,bound) = s
        let ni = match ctx.[str] with
                 | ValueNum n -> n
                 | _ -> failwith ""
        let ctx1 = Map.add str (ValueNum (ni - 1)) ctx
        (c,t,p,ctx1,bound)
    | Decrement(str,e) ->
        let (c,t,p,ctx,bound) = s
        let ni = match ctx.[str] with
                 | ValueNum n -> n
                 | _ -> failwith ""
        let ctx1 = Map.add str (ValueNum (ni - e)) ctx
        (c,t,p,ctx1,bound)
    | GoHome ->
        let (c,t,p,ctx,bound) = s
        let (d,h) = bound
        let (xhome,yhome) = h
        let (x,y,a) = t
        let t' = Turtle(xhome,yhome,(PI/2.0))
        let (wid,rgb,down) = p
        if down then
            let c' = Line(x,y,xhome,yhome,wid,rgb)::c
            (c',t',p,ctx,bound)
        else (c,t',p,ctx,bound)
    | SetHome(a,b) ->
        let acomp = getnumval a s
        if acomp < 1 then failwith "xcomp must be positive"
        let bcomp = getnumval b s
        if bcomp < 1 then failwith "ycomp must be positive"
        let (c,t,p,ctx,bound) = s
        let (d,h) = bound
        let h' = Home(acomp,bcomp)
        let bound' = (d,h')
        (c,t,p,ctx,bound')
    | SetDimensions(a,b) ->
        let acomp = getnumval a s
        if acomp < 1 then failwith "xcomp must be positive"
        let bcomp = getnumval b s
        if bcomp < 1 then failwith "ycomp must be positive"
        let (c,t,p,ctx,bound) = s
        let (d,h) = bound
        let d' = Dimensions(acomp,bcomp)
        let bound' = (d',h)
        (c,t,p,ctx,bound')
    | PenRGB(rarg,garg,barg) ->
        let rcomp = getnumval rarg s
        let gcomp = getnumval garg s
        let bcomp = getnumval barg s
        let (c,t,p,ctx,bound) = s
        let (w,rgb,d) = p
        let (r,g,b) = rgb
        let rgb' = (rcomp,gcomp,bcomp)
        (c,t,Pen(w,rgb',d),ctx,bound)