open System
open Parser
open LambdaParser
open AlphaReduction
open CS334

[<EntryPoint>]
let main argv =
    // testing sub #1
    let v = 'x'
    let with_e = Variable 'y'
    let in_e = Application(Variable 'x', Variable 'x')
    printfn "%s" (lambdaprint (sub v with_e in_e))

    // testing sub #2
    let v = 'x'
    let with_e = Variable 'y'
    let in_e = Abstraction('z', Application(Variable 'z', Variable 'x'))
    printfn "%s" (lambdaprint (sub v with_e in_e))

    // testing betastep #1: e = betastep e, where e is beta-normal
    let e = Abstraction('x', Variable 'x') // (Lx.x)
    printfn "%s" (lambdaprint (betastep e))

    // testing betastep #2:
    let e = Application(Abstraction('x',Application(Variable 'x', Variable 'y')), Abstraction('z', Application(Variable 'z', Variable 'y'))) // ((Lx.(xy))(Lz.(zy)))
    // one beta step should give us ((Lz.(zy))y)
    printfn "%s" (lambdaprint (betastep e))

    0
