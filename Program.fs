module Fox.Program

open System
open System.IO
open Core
open Interpreter
open StdLib
open Parser

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "Usage: fox <filename>"
        1
    else
        try
            let code = File.ReadAllText argv[0]
            let program = parseProgram code
            let _, result = runProgram program stdModules
            printfn "Result: %A" result
            0
        with e ->
            printfn "Error: %s" e.Message
            1
