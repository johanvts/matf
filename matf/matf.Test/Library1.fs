module Matf.Test

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Matf.AST
open Matf.Interpreter
open Matf.Parser

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
        let result1 = eval Map.empty (parse pArithmetic str1)
        let result2  = eval Map.empty (parse pArithmetic str2)
        result1 |> should (equalWithin 0.0000001) result2

[<Property( Arbitrary=[| typeof<LimRange>  |])>]
let``Addition Properties - Associative floats``(x,y,z) =
        let str1 = sprintf  "%f + ( %f + %f )" x y z
        let str2 = sprintf  "( %f + %f ) + %f" x y z
        let result1 = eval Map.empty (parse pArithmetic str1)
        let result2  = eval Map.empty (parse pArithmetic str2)
        result1 |> should (equalWithin 0.0000001) result2

let positive x = if x > 0.0000001 then true else false
    
[<Property( Arbitrary=[| typeof<LimRange>  |])>]
let``Addition Properties - Adding a positive floats grows the sum``(x, y) =
        positive y ==>
            let str1 = sprintf  "%f + %f" x y
            let result = eval Map.empty (parse pArithmetic str1)
            result > x

type ``Given addition``()=
    
    [<TestCase(10,4,14)>]
    [<TestCase(7000,2,7002)>]
    [<TestCase(10,45,55)>]
    [<TestCase(3,35.2,38.2)>]
    member t.``Addition gives correct results``(x,y,expected) =
        let testString = sprintf  "%f + %f" x y
        let actual = eval Map.empty (parse pArithmetic testString)
        actual |> should (equalWithin 0.0000001) expected

