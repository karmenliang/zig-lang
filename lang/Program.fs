open System
open System.Diagnostics
open System.IO
open ProjectParser
open ProjectInterpreter
open System.ComponentModel

let svgDraw guts =
    // default canvas size of 600x400px 
    let header = """<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" 
 "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">

<svg xmlns="http://www.w3.org/2000/svg" 
 xmlns:xlink="http://www.w3.org/1999/xlink" 
 width='600px' height='400px'>"""

    let footer = """</svg>"""
    header + guts + footer

let initGraphicsProcess(svgpath: string): Process =
    // "open" only works on the Mac
    // look to see if OS is Mac or Windows; Windows is "explorer.exe" Mac is "open"
    // still doesn't work
    let os = match int Environment.OSVersion.Platform with
             | 4 | 128 -> "open " // Linux
             | 6 -> "open " // OSX
             | _ -> "explorer.exe " // Windows
    let info = new ProcessStartInfo (
                 FileName = "/bin/bash",
                 Arguments = "-c \"" + os + svgpath + "\"",
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

let canvasSVGize (c:Canvas) : string =
    let rec pl c : (string*string*string*string*string*string) list =
       match c with
       | (x1,y1,x2,y2,wid,col)::ctail -> (x1.ToString(),y1.ToString(),x2.ToString(),y2.ToString(),wid.ToString(),col) :: (pl ctail)
       | [] -> []
    let rec svglist list : string =
        match list with
        | (x1,y1,x2,y2,wid,col)::tail -> "<line x1='" + x1 + "' x2='" + x2 + "' y1='" + y1 + "' y2='" + y2 + "' stroke-width='" + wid + "' stroke='" + col + "'/>" + (svglist tail)
        | [] -> ""
    svglist (pl c)  
let usage() =
    printfn "Usage: dotnet run \"<s>\", where <s> is a Turtle expression"
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
    let aState = State(List.empty,Turtle(300,200,(PI/2.0)),Pen(1,"black",true)) // default State

    // reading in .zig files
    if argv.[0].Contains ".zig" then 
        let readLines = File.ReadAllLines(argv.[0]) |> String.concat("")
        let altInput = parse readLines
        match altInput with
        | Some expr -> printfn "%s" (prettyprint expr)
        | None -> printfn "nope"
        let x = match altInput with
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
        Threading.Thread.Sleep(5000)
        0
    // reading in user input from command line
    else 
    let input = parse (argparse argv)

    // for debugging
    match input with
    | Some expr -> printfn "%s" (prettyprint expr)
    | None -> printfn "nope"

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