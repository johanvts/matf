// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "AST.fs"
#load "Interpreter.fs"
open AST
open Interpreter
open System.Collections.Generic
// Define your library scripting code here

let d = new Dictionary<identifier,value>()
eval d (Arithmetic(Literal 12.0, Add,Literal 2.0))
let square (a:double) =
    a*a
eval d (Sum(Literal 1.0,Literal 2.0,Func square))