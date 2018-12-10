namespace ProjectInterpreterTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter

[<TestClass>]
type InterpreterTestClass () =

    [<TestMethod>]
    member this.PencolorChangesState () =
        let input = PenRGB ((NumExpr 150),(NumExpr 100),(NumExpr 10))
        let expected = State(List.empty,Turtle(300,200,(PI/2.0)),Pen(1,(150,100,10),true),Map.empty,Boundaries(Dimensions(600,400),Home(300,200)))
        let origstate = State(List.empty,Turtle(300,200,(PI/2.0)),Pen(1,(0,0,0),true),Map.empty,Boundaries(Dimensions(600,400),Home(300,200)))
        let result = eval input origstate
        Assert.AreEqual(expected, result)
