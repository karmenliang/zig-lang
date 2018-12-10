namespace ProjectParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser

[<TestClass>]
type ParserTestClass () =

    [<TestMethod>]
    member this.SeqParses () =
        let input = "ahead 50; cw 90; ahead 50"
        let expected = Seq (Ahead (NumExpr 50),Seq (Clockwise (NumExpr 90),Ahead (NumExpr 50)))
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false
