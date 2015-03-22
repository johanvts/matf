module Matf.Parser

open Matf.AST

#if INTERACTIVE
#r @"..\..\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#endif
open FParsec

let test p str =
    match run p str with
    | Success(result,_,_) -> printf "Success: %s" result
    | Failure(errorMsg,_,_) -> printf "Failure: %s" errorMsg

//Using pName naming for our types to aviod any overlappign with default parsers

let pValue = pfloat

let pAdd = pstring "+"
let pSubtract = pstring "-"
let pMultiply = pstring @"\cdot" <|> pstring "*"
let pDivide = pstring @"\"

let pOperator = pAdd <|> pSubtract <|> pMultiply <|> pDivide

test pOperator "\cdot"


