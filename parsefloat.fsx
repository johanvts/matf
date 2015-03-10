#I @"/home/johanvts/Documents/documents/fsscripts/FParsec.1.0.1/lib/net40-client/"
#r "FParsecCS.dll"
open FParsec
#r "FParsec.dll"

type UserState = unit
type Parser<'t> = Parser<'t,UserState>

let test p str =
  match run p str with
    | Success(result,_,_) -> printfn "Success: %A" result
    | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg


let pbetween pStart pEnd p = pStart >>. p .>> pEnd
let pbetweenStrings s1 s2 p = p|> between (pstring s1) (pstring s2)

let pstringLiteral:Parser<_> =
  let normalChar = satisfy (fun c -> c<> '\\' && c <> '"' && c <> '^')
  manyChars normalChar
let pnumberLiteral:Parser<_> =
  pfloat |> string |> pstring 
let pliteral = pnumberLiteral <|> pstringLiteral
let ppow = pipe3 (pliteral) (pstring "^") (pliteral) (fun x _ y -> sprintf"pown %s %s" x y)
let pop:Parser<_> =
  pstring "+" <|> pstring "-" <|> pstring "\cdot" <|> pstring "/"

  
  

