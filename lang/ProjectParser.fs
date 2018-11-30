module ProjectParser

open Parser

// x, y, angle
type Turtle = int * int * float
// width, color, press
type Pen = int * string * bool
type Line = int * int * int * int * string
type Canvas = Line list
type State = Canvas * Turtle * Pen

type Expr =
| Ahead of int
| Behind of int
| Clockwise of int
| Counterwise of int
| Press
| Lift
| Seq of Expr*Expr
| Pencolor of string
| Loop of int*Expr

let expr, exprImpl = recparser()

let pposnumber = pmany1 pdigit |>> stringify |>> int <!> "pposnumber"
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber <!> "pnumber"
let pstring = pmany1 pletter |>> stringify <!> "pstring"
let pnuminparens = pbetween (pchar '(') (pchar ')') pnumber <!> "pnuminparens"
let pbrackets = pbetween (pchar '{') (pchar '}') expr <!> "pbrackets"

let ahead = pright (pstr ("ahead ")) pnumber |>> (fun a -> Ahead(a)) <!> "ahead"
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
let loop = pright (pstr ("loop ")) (pseq pnuminparens pbrackets (fun(i,e) -> Loop(i,e))) <!> "loop"
let nonrecexpr =  ahead <|> a <|> behind <|> b <|> clockwise <|> cw <|> counterwise <|> ccw <|> press <|> lift <|> pencolor <|> pc <|> loop <!> "nonrecexpr"

let seq = pseq (pleft nonrecexpr (pstr "; ")) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"


exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None