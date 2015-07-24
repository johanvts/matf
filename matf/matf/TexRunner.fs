module Matf.TexRunner

open Matf.Interpreter
open Matf.Parser

// LaTeX equation evaluation

let evalTex s =
    try
        string (eval Map.empty (parse pTex s))
    with
        | :? System.ArgumentException as ex -> ex.Message
        | ex -> ex.Message
