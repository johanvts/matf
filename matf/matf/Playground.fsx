// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "AST.fs"
#load "Interpreter.fs"
#load "Parser.fs"
open Matf.AST
open Matf.Interpreter
open Matf
open System.Collections.Generic
// Define your library scripting code here

eval Map.empty (Arithmetic(Literal 12.0, Add, Literal 2.0))

let square (a:double) =
    a * a

eval Map.empty (Let ("x", (Literal 1.0), Sum (Var "x", Literal 2.0, Func square)))
eval Map.empty (Let ("x", Literal 2.0, (Sum (Var "x", (Arithmetic (Var "x", Multiply, Var "x")), Var "x"))))
