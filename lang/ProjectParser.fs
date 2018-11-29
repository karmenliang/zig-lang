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
let behind = pright (pstr ("behind ")) pnumber |>> (fun a -> Behind(a)) <!> "behind"
let clockwise = pright (pstr ("clockwise ")) pnumber |>> (fun a -> Clockwise(a)) <!> "clockwise"
let press = (pstr "press" |>> fun a -> Press) <!> "press"
let lift = (pstr "lift" |>> fun a -> Lift) <!> "lift"
let pencolor = pright (pstr ("pencolor ")) pstring |>> (fun a -> Pencolor(a)) <!> "pencolor"
let loop = pright (pstr ("loop ")) (pseq pnuminparens pbrackets (fun(i,e) -> Loop(i,e))) <!> "loop"

let nonrecexpr =  ahead <|> behind <|> clockwise <|> press <|> lift <|> pencolor <|> loop <!> "nonrecexpr"

let seq = pseq (pleft nonrecexpr (pstr "; ")) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"


exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None