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

    0
