module ProjectParser

open Parser

//rgb(240, 200, 255)

type Expr =
| StringExpr of string
| NumExpr of int
| Ahead of Expr
| Behind of Expr
| Clockwise of Expr
| Counterwise of Expr
| Press
| Lift
| Seq of Expr*Expr
| Pencolor of string
| Penwidth of Expr
| Loop of Expr*Expr
| Assign of string*Expr
| UnaryIncrement of string
| Increment of string*int
| UnaryDecrement of string
| Decrement of string*int
| Penred of Expr
| Pengreen of Expr
| Penblue of Expr

type Value =
| ValueString of string
| ValueNum of int

// stores variables of type Value
type Context = Map<string,Value>

// x, y, angle
type Turtle = int * int * float
type RGB = int * int * int
// width, color, press
type Pen = int * RGB * bool
// x1, y1, x2, y2, width, RGB
type Line = int * int * int * int * int * RGB
type Canvas = Line list
type State = Canvas * Turtle * Pen * Context

let expr, exprImpl = recparser()

// HELPER PARSERS
let pposnumber = pmany1 pdigit |>> stringify |>> int <!> "pposnumber"
let pnumber = pright (pchar '-') (pposnumber)  |>> (fun n -> -n) <|> pposnumber <!> "pnumber"
let pnumberexpr = pnumber |>> (fun a -> NumExpr(a)) <!> "pnumberexpr"
let pstring = pmany1 pletter |>> stringify <!> "pstring"
let pstringexpr = pstring |>> (fun a -> StringExpr(a)) <!> "pstringexpr"
// value is either a string, number, or expression
let pvalue = pstringexpr <|> pnumberexpr <|> expr
let pnuminparens = pbetween (pchar '(') (pchar ')') pnumber <!> "pnuminparens"
let pbrackets = pbetween (pseq (pchar '{') (pseq (pmany0 pnl) pws0 (fun (a,b) -> null)) (fun (x,y)-> x)) (pseq (pmany0 pnl) (pchar '}') (fun (x,y)-> y)) expr <!> "pbrackets"

// EXPR PARSERS
let ahead = pright (pstr ("ahead ")) pvalue |>> (fun a -> Ahead(a)) <!> "ahead"
let a = pright (pstr ("a ")) pvalue |>> (fun a -> Ahead(a)) <!> "a"
let behind = pright (pstr ("behind ")) pvalue |>> (fun a -> Behind(a)) <!> "behind"
let b = pright (pstr ("b ")) pvalue |>> (fun a -> Behind(a)) <!> "b"
let clockwise = pright (pstr ("clockwise ")) pvalue |>> (fun a -> Clockwise(a)) <!> "clockwise"
let cw = pright (pstr ("cw ")) pvalue |>> (fun a -> Clockwise(a)) <!> "cw"
let counterwise = pright (pstr ("counterwise ")) pvalue |>> (fun a -> Counterwise(a)) <!> "counterwise"
let ccw = pright (pstr ("ccw ")) pvalue |>> (fun a -> Counterwise(a)) <!> "ccw"

let press = (pstr "press" |>> fun a -> Press) <!> "press"
let lift = (pstr "lift" |>> fun a -> Lift) <!> "lift"
let pencolor = pright (pstr ("pencolor ")) pstring |>> (fun a -> Pencolor(a)) <!> "pencolor"
let pc = pright (pstr ("pc ")) pstring |>> (fun a -> Pencolor(a)) <!> "pc"
let penred = pright (pstr ("penred ")) pvalue |>> (fun a -> Penred(a)) <!> "penred"
let pengreen = pright (pstr ("pengreen ")) pvalue |>> (fun a -> Pengreen(a)) <!> "pengreen"
let penblue = pright (pstr ("penblue ")) pvalue |>> (fun a -> Penblue(a)) <!> "penblue"
let penwidth = pright (pstr ("penwidth ")) pvalue |>> (fun a -> Penwidth(a)) <!> "penwidth"
let pw = pright (pstr ("pw ")) pvalue |>> (fun a -> Penwidth(a)) <!> "pw"
let loop = pright (pstr ("loop ")) (pseq (pleft (pbetween (pchar '(') (pchar ')') pvalue) pws0) pbrackets (fun(i,e) -> Loop(i,e))) <!> "loop"
let assign = pright (pstr "let ") (pseq (pleft pstring pws0) (pright (pstr "=") (pright pws0 pvalue)) (fun (str,e) -> Assign(str,e))) <!> "assign"
let unaryincrement = (pleft pstring (pstr "++") |>> fun (str) -> UnaryIncrement(str)) <!> "unaryincrement"
let increment = (pseq (pleft pstring pws0) (pright (pstr "+=") (pright pws0 pnumber)) (fun (str,e) -> Increment(str,e))) <!> "increment"
let unarydecrement = (pleft pstring (pstr "--") |>> fun (str) -> UnaryDecrement(str)) <!> "unarydecrement"
let decrement = (pseq (pleft pstring pws0) (pright (pstr "-=") (pright pws0 pnumber)) (fun (str,e) -> Decrement(str,e))) <!> "decrement"
let nonrecexpr =  ahead <|> a <|> behind <|> b <|> clockwise <|> cw <|> counterwise <|> ccw <|> press <|> lift <|> pencolor <|> pc <|> penred <|> pengreen <|> penblue <|> penwidth <|> pw <|> assign <|> unaryincrement <|> increment <|> unarydecrement <|> decrement <|> loop <!> "nonrecexpr"
let seq = pseq (pleft nonrecexpr (pseq (pstr ";") pws0 (fun (x,y) -> null))) expr (fun (e1,e2) -> Seq(e1,e2)) <!> "seq"
exprImpl := seq <|> nonrecexpr <!> "expr"
let grammar = pleft expr peof <!> "grammar"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None