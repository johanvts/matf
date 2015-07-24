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

let parse parser s =
    match run parser s with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg

let drops s = pstring s >>. spaces
let literal = pfloat .>> spaces |>> fun n -> Literal n
let str_ws s = pstring s .>> spaces

//Define the expression parser as forwarded

let pTexEq, pTexEqImpl = createParserForwardedToRef ()

//Parsing variables

let pName = FParsec.CharParsers.many1Chars letter 
let pUnbound = pName .>> spaces |>> fun name -> Var name
let pLet = pipe3 (pName.>> spaces .>> drops "=") (pTexEq .>> drops ",") (pTexEq)  (fun name value expr -> Let (name,value,expr))
let pVar = (attempt pLet) <|> pUnbound

//Sum and Prod functions

let betweenStr s1 s2 p = pstring s1 >>. p .>> pstring s2
let pLowerArgNa = ((pstring "_" >>? betweenStr "{" "}" pTexEq) <|> (pstring "_" >>. (literal <|> pVar))) .>> spaces |>> fun x -> ("na",x)
let pLowerArgName = pstring "_{" >>. spaces >>. pName .>> spaces .>> drops "=" .>>. pTexEq .>> pstring "}" .>> spaces
let pLowerArg = attempt (pLowerArgName) <|> pLowerArgNa
let pUpperArg = ((pstring "^" >>? betweenStr "{" "}" pTexEq) <|> (pstring "^" >>. (literal <|> pVar))) .>> spaces
let pFun f = 
        pstring f >>? (pipe3 (pLowerArg) (pUpperArg) (pTexEq) (fun (name,from) upto expr -> match f with
                                                                                            | @"\sum" -> Sum (name,from,upto,expr)
                                                                                            | @"\prod" -> Prod (name,from,upto,expr)
                                                                                            | _ -> failwith (sprintf "Unknown function %s" f)))
let pSum = pFun @"\sum"
let pProd = pFun @"\prod"


//Simpler functions

let pFrac = drops @"\frac{" >>.pTexEq .>> drops "}{" .>>.pTexEq .>>drops "}" 
            |>> fun (top,btn) -> Frac (top,btn)
let pLn = drops @"\ln{" >>.pTexEq .>>drops "}" |>> fun x -> Ln x 
let pSqrt = drops @"\sqrt{" >>. pTexEq .>>drops "}" |>> fun x -> Sqrt x


let pFunctions = pFrac <|> pLn  <|> pSqrt <|> pSum <|> pProd

//Set up operators with precedence
let opp = new OperatorPrecedenceParser<expr,unit,unit>()
let pArithmetic = opp.ExpressionParser
opp.TermParser <- literal <|> pVar <|> pFunctions <|> between (drops "(") (drops ")") pArithmetic
opp.AddOperator(InfixOperator("+", spaces,1,Associativity.Left, fun x y -> Arithmetic (x,Add,y)))
opp.AddOperator(InfixOperator("-", spaces,1,Associativity.Left, fun x y -> Arithmetic (x,Subtract,y)))
opp.AddOperator(InfixOperator("*", spaces,2,Associativity.Left, fun x y -> Arithmetic (x,Multiply,y)))
opp.AddOperator(InfixOperator("/", spaces,2,Associativity.Left, fun x y -> Arithmetic (x,Divide,y)))
opp.AddOperator(InfixOperator("^", spaces,3,Associativity.Right, fun x y -> Pow (x,y)))
opp.AddOperator(PrefixOperator("-", spaces,4,true, fun x -> Neg x))


//finally collect everything into an LatexEquation parser
pTexEqImpl := pArithmetic

let pTex = pTexEq .>> spaces .>> eof