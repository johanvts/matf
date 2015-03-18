module Interpreter

open AST
open System.Collections.Generic //Hashtable for storing variables

type VarLookUp = Dictionary<identifier, value>

let arithmetic left operator right = 
    match operator with
    | Add -> left + right
    | Subtract -> left - right
    | Multiply -> left * right
    | Divide -> left / right

let rec eval state (expr : expr) = 
    let (vars : VarLookUp) = state
    match expr with
    | Literal x -> x
    | Var identifier -> vars.[identifier]
    | Arithmetic(left, operator, right) ->
        arithmetic (eval state left) operator (eval state right)
    | Sum(from, upto, Func func) ->
        [ (eval state from)..(eval state upto) ] |> List.sumBy func
    | Sum(from, upto, a) ->
        [ (eval state from)..(eval state upto) ] |> List.sumBy (fun _ -> eval state a)
    | Prod(from, upto, Func func) ->
        [ (eval state from)..(eval state upto) ] |> List.fold (fun s i -> s * (func i)) 1.0
    | Prod(from, upto, a) ->
        [ (eval state from)..(eval state upto) ] |> List.fold (fun s _ -> s * (eval state a)) 1.0
    | Frac(top, btn) ->
        (eval state top) / (eval state btn)
    | Func(_) ->
        raise (System.ArgumentException("A function with no input is not a valid argument."))
