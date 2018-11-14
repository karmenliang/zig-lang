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

let ahead = pright pws0