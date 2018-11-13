open System
open Parser
open LambdaParser
open AlphaReduction
open CS334

let usage() =
    printfn "Usage: dotnet run <s>, where s is a lambda expression"
    exit 1

let argparse argv =
    if Array.length argv <> 1 then usage()
    let n =
        try string argv.[0]
        with
        | :? System.FormatException as e ->
            usage()   
    n

[<EntryPoint>]
let main argv =
    let input = parse (argparse argv)
    match input with
    | Some expr -> printfn "%A" (lambdaprint (betanorm expr))
    | None -> printfn "Invalid expression!"
    0
