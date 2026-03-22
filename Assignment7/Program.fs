// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State

let runProgram prog =
    42 |>
    Some |>
    mkState 10 |>
    stmntEval prog |>
    ignore

// runProgram guessANumber
    0
// Uncomment the program you want to run

//runProgram bubbleSort