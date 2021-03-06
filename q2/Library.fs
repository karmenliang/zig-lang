module CS334

open LambdaParser
open AlphaReduction
open LambdaParser

let rec sub (v:char) (with_e:Expr) (in_e:Expr) : Expr =
    match in_e with
    | Variable var -> if var = v then with_e
                      else Variable var
    // if alpha-normal, inner abstraction should have different
    // bound variable then outer abstraction
    | Abstraction(var,e) -> Abstraction(var, sub v with_e e)
    | Application(e1,e2) -> Application(sub v with_e e1, sub v with_e e2)

let rec betastep (e:Expr) : Expr =
    match e with
    | Variable v -> e
    | Abstraction(v,e') -> Abstraction(v, betastep e')
    | Application(e1,e2) ->
        match e1 with
        | Variable e1v -> Application(e1, betastep e2)
        | Abstraction(e1v, e1e) -> sub e1v e2 e1e
        | Application(e1',e2') ->
            // try reducing e1 (left) first
            let red1 = betastep e1
            if e1 <> red1 then Application(red1,e2)
            // if not reducible, try reducing e2' (right)
            else let red2 = betastep e2
                 if e2 <> red2 then Application(e1,red2)
                 // both subexpressions not reducible, return original
                 else e
            
let rec betanorm (e:Expr) : Expr =
    let expr = (fst (alphanorm e (fv e) Map.empty))
    let redexpr = betastep expr
    // in beta-normal form
    if expr = redexpr then redexpr
    // not in beta-normal form, continue beta reducing
    else betanorm redexpr
    