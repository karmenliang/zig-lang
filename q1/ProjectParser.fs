module ProjectParser

open Parser

type Color =
| Black
type Turtle = int * int * float
type Pen = int * Color * bool
type Line = int * int * int * int
type Canvas = Line list
type State = Canvas * Turtle * Pen

type Expr = 
| Ahead of int

let expr, exprImpl = recparser()

let pposnumber = pmany1 pdigit |>> stringify |>> int
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber

let ahead = (pright (pstr ("ahead ")) pnumber) |>> (fun a -> Ahead(a))

exprImpl := ahead

let grammar = pleft expr peof

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None