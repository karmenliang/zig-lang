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

[<EntryPoint>]
let main argv =
    let svg = svgDraw (
                (polyline [(300,200);(300,250);(350,250);(350,200);(300,200)] 1 "green")
              )

    printfn "Writing an SVG to a file and opening with your web browser..."
    File.WriteAllText("output.svg", svg)
    displaySVG "output.svg"
    Threading.Thread.Sleep(5000)  // wait for the web browser to start up
    //File.Delete "output.svg" // cleanup
    0
