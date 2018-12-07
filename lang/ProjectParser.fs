module ProjectParser

open Parser

//rgb(240, 200, 255)

type Expr =
| StringVal of string
| NumVal of int
//| Variable of string
| Ahead of int
| AheadVar of string
| Behind of int
| Clockwise of int
| Counterwise of int
| Press
| Lift
| Seq of Expr*Expr
| Pencolor of string
| Penwidth of int
| Loop of int*Expr
| Assign of string*Expr
| UnaryIncrement of string
| Increment of string*int
| Penred of int
| Penredvar of string
| Pengreen of int
| Penblue of int

type Value =
| ValueString of string
| ValueNum of int
//| ValueExpr of Expr

type Context = Map<string,Value>

// x, y, angle
type Turtle = int * int * float
type RGB = int * int * int
// width, color, press
type Pen = int * RGB * bool
// x1, y1, x2, y2, width, color name
type Line = int * int * int * int * int * RGB
type Canvas = Line list
type State = Canvas * Turtle * Pen * Context

let expr, exprImpl = recparser()

// helper parsers
let pposnumber = pmany1 pdigit |>> stringify |>> int <!> "pposnumber"
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber <!> "pnumber"
let pnumberval = pnumber |>> (fun a -> NumVal(a)) <!> "pnumberval"
let pstring = pmany1 pletter |>> stringify <!> "pstring"
let pstringval = pstring |>> (fun a -> StringVal(a)) <!> "pstringval"
// let pvar = pnumberval <|> pstringval
let pnuminparens = pbetween (pchar '(') (pchar ')') pnumber <!> "pnuminparens"
let pbrackets = pbetween (pseq (pchar '{') (pseq (pmany0 pnl) pws0 (fun (a,b) -> null)) (fun (x,y)-> x)) (pseq (pmany0 pnl) (pchar '}') (fun (x,y)-> y)) expr <!> "pbrackets"

let ahead = pright (pstr ("ahead ")) pnumber |>> (fun a -> Ahead(a)) <!> "ahead"
let aheadvar = pright (pstr ("ahead ")) pstring |>> (fun a -> AheadVar(a)) <!> "ahead"
let a = pright (pstr ("a ")) pnumber |>> (fun a -> Ahead(a)) <!> "a"
let behind = pright (pstr ("behind ")) pnumber |>> (fun a -> Behind(a)) <!> "behind"
let b = pright (pstr ("b ")) pnumber |>> (fun a -> Behind(a)) <!> "b"
let clockwise = pright (pstr ("clockwise ")) pnumber |>> (fun a -> Clockwise(a)) <!> "clockwise"
let cw = pright (pstr ("cw ")) pnumber |>> (fun a -> Clockwise(a)) <!> "cw"
let counterwise = pright (pstr ("counterwise ")) pnumber |>> (fun a -> Counterwise(a)) <!> "counterwise"
let ccw = pright (pstr ("ccw ")) pnumber |>> (fun a -> Counterwise(a)) <!> "ccw"
let press = (pstr "press" |>> fun a -> Press) <!> "press"
let lift = (pstr "lift" |>> fun a -> Lift) <!> "lift"
let pencolor = pright (pstr ("pencolor ")) pstring |>> (fun a -> Pencolor(a)) <!> "pencolor"
let pc = pright (pstr ("pc ")) pstring |>> (fun a -> Pencolor(a)) <!> "pc"
let penred = pright (pstr ("penred ")) pnumber |>> (fun a -> Penred(a)) <!> "penred"
let penredvar = pright (pstr ("penred ")) pstring |>> (fun a -> Penredvar(a)) <!> "penred"

let pengreen = pright (pstr ("pengreen ")) pnumber |>> (fun a -> Pengreen(a)) <!> "pengreen"
let penblue = pright (pstr ("penblue ")) pnumber |>> (fun a -> Penblue(a)) <!> "penblue"
let penwidth = pright (pstr ("penwidth ")) pposnumber |>> (fun a -> Penwidth(a)) <!> "penwidth"
let pw = pright (pstr ("pw ")) pposnumber |>> (fun a -> Penwidth(a)) <!> "pw"
let loop = pright (pstr ("loop ")) (pseq (pleft pnuminparens pws0) pbrackets (fun(i,e) -> Loop(i,e))) <!> "loop"
// value is either a string, number, or expression
let value = pstringval <|> pnumberval <|> expr

let assign = pright (pstr "let ") (pseq (pleft pstring pws0) (pright (pstr "=") (pright pws0 value)) (fun (str,e) -> Assign(str,e))) <!> "assign"

let unaryincrement = (pleft pstring (pstr "++") |>> fun (str) -> UnaryIncrement(str)) <!> "increment"
let increment = (pseq (pleft pstring pws0) (pright (pstr "+=") (pright pws0 pnumber)) (fun (str,e) -> Increment(str,e))) <!> "increment"
let nonrecexpr =  ahead <|> aheadvar <|> a <|> behind <|> b <|> clockwise <|> cw <|> counterwise <|> ccw <|> press <|> lift <|> pencolor <|> pc <|> penred <|> penredvar <|> pengreen <|> penblue <|> penwidth <|> pw <|> assign <|> unaryincrement <|> increment <|> loop <!> "nonrecexpr"
let seq = pseq (pleft nonrecexpr (pseq (pstr ";") pws0 (fun (x,y) -> null))) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"
exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None