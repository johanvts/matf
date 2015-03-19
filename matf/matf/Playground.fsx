// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "AST.fs"
#load "Interpreter.fs"
open AST
open Interpreter
open System.Collections.Generic
// Define your library scripting code here

eval [] (Arithmetic(Literal 12.0, Add, Literal 2.0))

let square (a:double) =
    a * a

eval [] (Let ("x", (Literal 1.0), Sum (Var "x", Literal 2.0, Func square)))
eval [] (Let ("x", Literal 2.0, (Sum (Var "x", (Arithmetic (Var "x", Multiply, Var "x")), Var "x"))))
