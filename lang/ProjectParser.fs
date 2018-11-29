module ProjectParser

open Parser

type Color =
| Black
| Red
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

let expr, exprImpl = recparser()

let pposnumber = pmany1 pdigit |>> stringify |>> int <!> "pposnumber"
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber <!> "pnumber"
let pstring = pmany1 pletter |>> stringify <!> "pstring"
let pencolor = pright (pstr ("pencolor ")) pstring |>> (fun a -> Pencolor(a)) <!> "pencolor"

let ahead = pright (pstr ("ahead ")) pnumber |>> (fun a -> Ahead(a)) <!> "ahead"
let behind = pright (pstr ("behind ")) pnumber |>> (fun a -> Behind(a)) <!> "behind"
let clockwise = pright (pstr ("clockwise ")) pnumber |>> (fun a -> Clockwise(a)) <!> "clockwise"
let press = (pstr "press" |>> fun a -> Press) <!> "press"
let lift = (pstr "lift" |>> fun a -> Lift) <!> "lift"

let nonrecexpr =  ahead <|> behind <|> clockwise <|> press <|> lift <|> pencolor <!> "nonrecexpr"

let seq = pseq (pleft nonrecexpr (pstr "; ")) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"


exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None