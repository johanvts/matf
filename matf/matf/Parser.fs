﻿module Matf.Parser

#if INTERACTIVE
#r @"..\..\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#endif

open Matf.AST
open FParsec

let test p str =
    match run p str with
    | Success(result,_,_) -> printf "Success: %s" result
    | Failure(errorMsg,_,_) -> printf "Failure: %s" errorMsg

let parse parser s =
    match run parser s with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg

let drops s = pstring s >>. spaces
let literal = pfloat .>> spaces |>> fun n -> Literal n

//set up operators with precedence
let opp = new OperatorPrecedenceParser<expr,unit,unit>()
let pArithmetic = opp.ExpressionParser
opp.TermParser <- literal <|> between (drops "(") (drops ")") pArithmetic
opp.AddOperator(InfixOperator("+", spaces,1,Associativity.Left, fun x y -> Arithmetic (x,Add,y)))
opp.AddOperator(InfixOperator("-", spaces,1,Associativity.Left, fun x y -> Arithmetic (x,Subtract,y)))
opp.AddOperator(InfixOperator("*", spaces,2,Associativity.Left, fun x y -> Arithmetic (x,Multiply,y)))
opp.AddOperator(InfixOperator("/", spaces,2,Associativity.Left, fun x y -> Arithmetic (x,Divide,y)))
opp.AddOperator(InfixOperator("^", spaces,3,Associativity.Right, fun x y -> Pow (x,y)))
opp.AddOperator(PrefixOperator("-", spaces,4,true, fun x -> Neg x))



