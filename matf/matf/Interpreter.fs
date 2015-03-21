module Matf.Interpreter

open Matf.AST

let arithmetic left operator right =
    match operator with
    | Add -> left + right
    | Subtract -> left - right
    | Multiply -> left * right
    | Divide -> left / right

let rec eval state (expr : expr) =
    match expr with
    | Literal x -> x
    | Var name ->
        match state |> List.tryFind (fun (k, _) -> k = name) with
        | Some x -> snd x
        | None -> failwith "Unbound variable."
    | Let (name, e1, e2) -> eval ((name, eval state e1) :: state) e2
    | Where (e1, name, e2) -> eval state (Let (name, e2, e1))
    | Arithmetic(left, operator, right) ->
        arithmetic (eval state left) operator (eval state right)
    | Sum (from, upto, Func func) ->
        [ (eval state from)..(eval state upto) ] |> List.sumBy func
    | Sum (from, upto, a) ->
        [ (eval state from)..(eval state upto) ] |> List.sumBy (fun _ -> eval state a)
    | Prod (from, upto, Func func) ->
        [ (eval state from)..(eval state upto) ] |> List.fold (fun s i -> s * (func i)) 1.0
    | Prod (from, upto, a) ->
        [ (eval state from)..(eval state upto) ] |> List.fold (fun s _ -> s * (eval state a)) 1.0
    | Frac (top, btn) ->
        (eval state top) / (eval state btn)
    | Pow (e1, e2) -> (eval state e1) ** (eval state e2)
    | Sqrt e -> sqrt (eval state e)
    | Func (_) ->
        failwith "A function with no input is not a valid argument."
