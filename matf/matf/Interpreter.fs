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
    | Literal x -> 
        match x with
        | x when System.Double.IsNaN(x) -> failwith "To small to compute. Float NaN"
        | x when System.Double.IsInfinity(x) -> failwith "To large to compute. Float inf."
        | x -> x
    | Var name ->
        match state |> Map.tryFind name with
        | Some x -> x
        | None -> failwith (sprintf "Unbound variable %s." name)
    | Let (name, e1, e2) -> eval (state |> Map.add name (eval state e1)) e2
    | Arithmetic(left, operator, right) ->
        arithmetic (eval state left) operator (eval state right)
    | Sum (name, from, upto, exp) ->
        [ (eval state from)..(eval state upto) ] |> List.sumBy (fun value -> eval (state |> Map.add name value) exp)
    | Prod (name, from, upto, exp) ->
        [ (eval state from)..(eval state upto) ] |> List.fold (fun s value -> s * (eval (state |> Map.add name value) exp)) 1.0
    | Frac (top, btn) ->
        (eval state top) / (eval state btn)
    | Ln expr -> log (eval state expr)
    | Pow (e1, e2) -> (eval state e1) ** (eval state e2)
    | Sqrt e -> sqrt (eval state e)
    | Neg x -> -(eval state x)
    | Sin x -> System.Math.Sin(eval state x)
    | Cos x -> System.Math.Cos(eval state x)
    | Tan x -> System.Math.Tan(eval state x)