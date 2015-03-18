module AST

//Type abbriviations
type label = string
type identifier = string

//Latex values
type value = double

//Latex operators
type arithmetic =
    |Add |Subtract |Multiply |Divide 


//Latex expression
type expr =
    |Literal of value
    |Var of identifier
    |Arithmetic of expr * arithmetic * expr
    |Func of (value -> value)
    |Sum of expr * expr * expr
    |Prod of expr * expr * expr
    |Frac of expr * expr
    

