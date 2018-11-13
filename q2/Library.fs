module CS334

open LambdaParser

let rec sub (v:char) (with_e:Expr) (in_e:Expr) : Expr =
    match in_e with
    | Variable var -> if var = v then with_e
                      else Variable var
    // if alpha-normal, inner abstraction should have different
    // bound variable then outer abstraction
    | Abstraction(var,e) -> Abstraction(var, sub v with_e e)
    | Application(e1,e2) -> Application(sub v with_e e1, sub v with_e e2)