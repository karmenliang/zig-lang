namespace ProjectInterpreterTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type InterpreterTestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
