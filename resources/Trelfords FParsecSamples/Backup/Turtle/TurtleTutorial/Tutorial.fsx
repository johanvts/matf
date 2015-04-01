// Welcome to the Turtle Tutorial
// To run code fragments, select code then either:
// - press ALT+ENTER (Visual Studio) or CTRL+ENTER (Xamarin/MonoDevelop)
// - right click & "Execute in Interactive"

#r @"../../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"
#load "AST.fs"
#load "Interpreter.fsx"
open AST
open Interpreter

// ***************************************************
// Task 0 - Execute the line below in F# interactive
// ***************************************************

execute [Repeat(36,[Forward 10;Turn 10])]

// ***************************************************
// Task 1 - Parsing numbers
// ***************************************************

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "__"

// ***************************************************
// Task 2 - Parse "fd"
// ***************************************************

let fd = pstring "fd"
test fd "__"

// ***************************************************
// Task 3 - Parsing "forward 10"
// ***************************************************

let forward = pstring "forward" >>. spaces >>. pfloat
test forward "forward 10"

// ***************************************************
// Task 4 - Parsing "forward10" should fail
// Hint: spaces1 requires atleast 1 white space character
// ***************************************************

test forward "forward10"

// ***************************************************
// Task 5 - Parsing to AST - Forward, Left, Right
// ***************************************************

let pfwd = forward |>> fun n -> Forward(int n)
test pfwd "forward 10"
// TODO: complete left & right parsers
// let left = pstring "left" >>. spaces1 >>. pfloat 
// let plt = left ...
// test plt "lt 10
// let prt = ...
// test prt "rt 10"

// ***************************************************
// Task 6 - Parsing fd or forward, lt or left etc.
// Hint: use (p <|> p') for choice
// ***************************************************

let pforward = (pstring "fd" <|> pstring "forward") >>. spaces1 >>. pfloat
               |>> fun n -> Forward(int n)

test pforward "forward 10"
test pforward "fd 10"
// TODO:
// let pleft =
// test pleft "lt 10"
// let pright =
// test pright "rt 10"

// ***************************************************
// Task 7 - Parsing commands
// Hint: use (p <|> p') for choice
// ***************************************************

// TODO: handle forward, left or right
let pcommand = pforward // ...

test pcommand "forward 10"
test pcommand "left 90"
test pcommand "rt 180"

// ***************************************************
// Task 8 - Parsing multiple commands
// Hint: use many
// ***************************************************

let pparse = many pcommand

test pparse "forward 10forward 10"

// ***************************************************
// Task 9 - Parsing multiple commands with spaces
// Hint: use spaces function
// ***************************************************

let pparsews = many (pcommand (*TODO:handle spaces*))
test pparsews "forward 10 right 90 forward 10"

// ***************************************************
// Task 10 - Drawing a triangle
// Note: uses run function
// ***************************************************

let parse parser s = 
    match run parser s  with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg

let triangle = parse pparsews "forward 50 right 120 forward 50"
execute triangle

// ***************************************************
// Task 11 - Parsing "[forward 50]"
// Hint: use many and spaces1
// ***************************************************

let pblock1 = between (pstring "[") (pstring "]") pcommand
test pblock1 "[forward 50]"

// ***************************************************
// Task 12 - Parsing "[forward 50 right 90]"
// ***************************************************

let pblock = between (pstring "[") (pstring "]") ( (*TODO: handle many with ws*) pcommand)
test pblock "[forward 50 right 90]"

// ***************************************************
// Task 13 - Parsing "repeat 36 [forward 10 right 10]"
// ***************************************************

// TODO: uncomment this
//let prepeat = pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. pblock
//              |>> fun (n,commands) -> Repeat(int n,commands)
//test prepeat "repeat 36 [forward 10 right 10]"

//let circle = [parse prepeat "repeat 36 [forward 10 right 10]"]
//execute circle

// ***************************************************
// Additional Task 13 - Parse "repeat 10 [right 36 repeat 5 [forward 54 right 72]]"
// Note: see createParserForwardedToRef ()
// * This one is a little tricky
// ***************************************************

// ...
