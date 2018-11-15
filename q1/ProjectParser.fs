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
| Ahead of char list

let expr, exprImpl = recparser()

let ahead = (pright (pstr ("ahead ")) (pmany1 pdigit)) |>> (fun a -> Ahead(a))

exprImpl := ahead

let grammar = pleft expr peof

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None