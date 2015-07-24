module Matf.Test

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Matf.AST
open Matf.Interpreter
open Matf.Parser
open Matf.TexRunner

//Floats limited to between -1000000 and 1000000 and precsions set to 0.0000001

type LimRange =
    static member Float() =
        Arb.Default.Float()
        |> Arb.filter (fun f -> not <| System.Double.IsNaN(f) &&
                                not <| System.Double.IsInfinity(f) &&
                                not <| (f > 1000000.0) &&
                                not <| (f < -1000000.0)
                                ) 

[<Property( Arbitrary=[| typeof<LimRange>  |])>]
let``Addition Properties - Commutative floats``(x,y) =
        let str1 = sprintf  "%f + %f" x y
        let str2 = sprintf  "%f + %f" y x
        let result1 = eval Map.empty (parse pTexEq str1)
        let result2  = eval Map.empty (parse pTexEq str2)
        result1 |> should (equalWithin 0.0000001) result2

[<Property( Arbitrary=[| typeof<LimRange>  |])>]
let``Addition Properties - Associative floats``(x,y,z) =
        let str1 = sprintf  "%f + ( %f + %f )" x y z
        let str2 = sprintf  "( %f + %f ) + %f" x y z
        let result1 = eval Map.empty (parse pTexEq str1)
        let result2  = eval Map.empty (parse pTexEq str2)
        result1 |> should (equalWithin 0.0000001) result2

let positive x = if x > 0.0000001 then true else false
    
[<Property( Arbitrary=[| typeof<LimRange>  |])>]
let``Addition Properties - Adding a positive floats grows the sum``(x: float, y:float) =
        let str1 = sprintf  "%f + %f" x y
        let result = eval Map.empty (parse pTexEq str1)
        positive y ==> (result > x) 

type ``Given addition``()=
    
    [<TestCase(10,4,14)>]
    [<TestCase(7000,2,7002)>]
    [<TestCase(10,45,55)>]
    [<TestCase(3,35.2,38.2)>]
    member t.``Addition gives correct results``(x,y,expected) =
        let testString = sprintf  "%f + %f" x y
        let actual = eval Map.empty (parse pTexEq testString)
        actual |> should (equalWithin 0.0000001) expected

type ``Given Let``() =
    
    [<TestCase(10,4,14)>]
    [<TestCase(0.2,4.14,4.34)>]
    member t.``Variable by Let and use in addition`` (x,y,expected) =
        let testString = sprintf "x = %f, x + %f" x y
        let actual = eval Map.empty (parse pTexEq testString)
        actual |> should (equalWithin 0.0000001) expected
    [<TestCase(10,4,14)>]
    [<TestCase(0.2,4.14,4.34)>]
    member t.``Variable by Let - multiple use in addition`` (x,y,expected) =
        let testString = sprintf "x = %f, y = %f, x + y" x y
        let actual = eval Map.empty (parse pTexEq testString)
        actual |> should (equalWithin 0.0000001) expected

type ``Given Sum``() =

    [<TestCase(0,4,5)>]
    member t.``Sum - simple`` (from,upto,expected) =
        let testString = sprintf "\sum_%f^{%f} 1" from upto
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

    [<TestCase(0,4,10)>]
    member t.``Sum - named`` (from,upto,expected) =
        let testString = sprintf "\sum_{i=%f}^{%f} i" from upto
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

    [<TestCase(0,4,30)>]
    [<TestCase(43,65,68080)>]
    member t.``Sum - of Squares`` (from,upto,expected) =
        let testString = sprintf "\sum_{i=%f}^{%f} i^2" from upto
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected
       
type ``Given Prod``() =

    [<TestCase(0,4,0)>]
    [<TestCase(1,4,24)>]
    member t.``Prod - simple`` (from,upto,expected) =
        let testString = sprintf "\prod_{i=%f}^{%f} i" from upto
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

type ``Given Frac``() =
    
    [<TestCase(4,2,2)>]
    member t.``Frac - simple`` (top,btn,expected) =
        let testString = sprintf @"\frac{%f}{%f}" top btn
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

type ``Given Ln``() =
    
    [<TestCase(4,1.3862943611198906188344642429163531361510002687205105)>]
    [<TestCase(1,0)>]
    member t.``Ln - Simple`` (x,expected) =
        let testString = sprintf @"\ln{%f}" x
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

type ``Given Sqrt``() =
    
    [<TestCase(9,3)>]
    [<TestCase(2,1.4142135623730950488016887242096980785696718753769480)>]
    [<TestCase(0,0)>]
    member t.``Sqrt - Simple`` (x,expected) =
        let testString = sprintf @"\sqrt{%f}" x
        let acutal = eval Map.empty (parse pTexEq testString)
        acutal |> should (equalWithin 0.0000001) expected

type ``Given combinated expressions``() =
    
    [<TestCase(0,2,21)>]
    [<TestCase(2,0,4)>]
    member t.``Combination - Var,Sum and square`` (a,b,expected) =
        let testString = sprintf "pft = %f, 2^pft + \sum_1^10 %f" a b
        let actual  = eval Map.empty (parse pTexEq testString)
        actual |> should (equalWithin 0.0000001) expected

type ``Given string interpretation``() =
    
    [<Test>]
    member t.``String with unbound variable`` () =
        let testString = sprintf "2^pft + \sum_1^10 5"
        let actual  = evalTex testString
        actual |> should equal "Unbound variable pft."