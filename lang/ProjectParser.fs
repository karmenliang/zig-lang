module ProjectParser

open Parser

type Color =
| Black
// x, y, angle
type Turtle = int * int * float
// width, color, pendown
type Pen = int * Color * bool
type Line = int * int * int * int
type Canvas = Line list
type State = Canvas * Turtle * Pen

type Expr =
| Ahead of int
| Behind of int
| Clockwise of int
| Pendown
| Penup
| Seq of Expr*Expr

let expr, exprImpl = recparser()

let pposnumber = pmany1 pdigit |>> stringify |>> int <!> "pposnumber"
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber <!> "pnumber"

let ahead = pright (pstr ("ahead ")) pnumber |>> (fun a -> Ahead(a)) <!> "ahead"
let behind = pright (pstr ("behind ")) pnumber |>> (fun a -> Behind(a)) <!> "behind"
let clockwise = pright (pstr ("clockwise ")) pnumber |>> (fun a -> Clockwise(a)) <!> "clockwise"
let pendown = (pstr "pendown" |>> fun a -> Pendown) <!> "pendown"
let penup = (pstr "penup" |>> fun a -> Penup) <!> "penup"

let nonrecexpr =  ahead <|> behind <|> clockwise <|> pendown <|> penup <!> "nonrecexpr"

let seq = pseq (pleft nonrecexpr (pstr "; ")) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"


exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None