// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics
open System.IO
open ProjectParser
open ProjectInterpreter

let polyline xys width color =
    let rec pl xys : string list =
       match xys with
       | (x,y)::xys' -> x.ToString() + " " + y.ToString() :: (pl xys')
       | [] -> []
    "<polyline points='" +
    String.Join(",", List.rev (pl xys)) + "' " +
    "stroke-width='" + width.ToString() + "' " +
    "stroke='" + color + "' " +
    "style='fill: none;' " +
    "/>"

let svgDraw guts =
    // The triple-double quote thing is called a "heredoc".
    // It lets us write long string literals that include
    // newlines, etc.
    let header = """<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" 
 "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">

<svg xmlns="http://www.w3.org/2000/svg" 
 xmlns:xlink="http://www.w3.org/1999/xlink" 
 width='600px' height='400px'>"""

    let footer = """</svg>"""
    header + guts + footer

let initGraphicsProcess(svgpath: string): Process =
    // "open" only works on the Mac
    let info = new ProcessStartInfo (
                 FileName = "/bin/bash",
                 Arguments = "-c \"open " + svgpath + "\"",
                 RedirectStandardOutput = true,
                 UseShellExecute = false,
                 CreateNoWindow = true
               )
    let p = new Process()
    p.StartInfo <- info
    p

let displaySVG(svgpath: string) : unit =
    let p = initGraphicsProcess svgpath
    if p.Start() then
        let result = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
    else
        printfn "Could not open SVG."
        exit 1

let rec canvasSVGize (c:Canvas) : string =
    let rec pl c : (string * string * string * string) list =
       match c with
       | (x,y,x',y')::ctail -> (x.ToString(),y.ToString(), x'.ToString(),y'.ToString()) :: (pl ctail)
       | [] -> []
    let (x1,y1,x2,y2) = (pl c).Head
    "<line x1='" + x1 + "' x2='" + x2 + "' y1='" + y1 + "' y2='" + y2 + 
    "' stroke-width='1' stroke='black'/>"
    
let usage() =
    printfn "Usage: dotnet run \"<s>\", where s is a Turtle expression"
    exit 1

let argparse argv =
    if Array.length argv <> 1 then
        usage()
    
    let n =
        try
            string argv.[0]
        with
        | :? System.FormatException as e ->
            usage()
    n

[<EntryPoint>]
let main argv =
    let aState = State(List.empty,Turtle(300,200,0.0),Pen(1,Black,true)) // default State

    let input = parse (argparse argv)

    let x = match input with
            | Some expr ->  (eval expr aState)
            | None -> aState
    let (c,_,_) = x
    let aCanvas = c
    
    let svg = svgDraw (
                (canvasSVGize aCanvas)
              )

    printfn "Writing an SVG to a file and opening with your web browser..."
    File.WriteAllText("output.svg", svg)
    displaySVG "output.svg"
    Threading.Thread.Sleep(5000)  // wait for the web browser to start up
    //File.Delete "output.svg" // cleanup
    0
