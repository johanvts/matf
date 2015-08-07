module Matf.AST

//Type abbriviations
type label = string
type name = string

//Latex values
type value = double

//Latex operators
type arithmetic =
    | Add
    | Subtract
    | Multiply
    | Divide

//Latex expression
type expr =
    | Literal of value
    | Var of name
    | Let of name * expr * expr
    | Arithmetic of expr * arithmetic * expr
    | Sum of name * expr * expr * expr
    | Prod of name * expr * expr * expr
    | Frac of expr * expr
    | Ln of expr
    | Pow of expr * expr
    | Sqrt of expr
    | Neg of expr
    | Sin of expr
    | Cos of expr
    | Tan of expr
