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
| Seq of Expr*Expr
| Ahead of int
| Behind of int
| Clockwise of int
| Pendown
| Penup

let expr, exprImpl = recparser()

let pposnumber = pmany1 pdigit |>> stringify |>> int
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber

let seq = pseq (pleft expr pws1) expr (fun (e1,e2) -> Seq(e1,e2))
// pseq expr expr (fun (e1,e2) -> Seq(e1,e2))


let ahead = pright (pstr ("ahead ")) pnumber |>> (fun a -> Ahead(a))
let behind = pright (pstr ("behind ")) pnumber |>> (fun a -> Behind(a))
let clockwise = pright (pstr ("clockwise ")) pnumber |>> (fun a -> Clockwise(a))
let pendown = pstr "pendown" |>> fun a -> Pendown
let penup = pstr "penup" |>> fun a -> Penup

exprImpl := seq <|> ahead <|> behind <|> clockwise <|> pendown <|> penup
let grammar = pleft expr peof

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None