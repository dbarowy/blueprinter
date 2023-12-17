open Parser
open Evaluator
open AST
open System.IO
open Tests


(* readProgram
 *   reads a program from a .bp file if the file exists
 *)
let readProgram(program: string): string =
    try
        File.ReadAllText program
    with
    | :? System.IO.FileNotFoundException as ex ->
        printfn "File not found: %s" ex.Message
        exit 1
    | :? System.IO.IOException as ex ->
        printfn "Error reading file: %s" ex.Message
        exit 1


[<EntryPoint>]
let main argv : int =
    (* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> [debug] or dotnet run --test"
        exit 1

    (* does the user want parser debugging turned on or to run the test suite? *)
    let do_debug = if (argv.Length = 2 && argv[1] = "debug") then true else false
    let do_test = if (argv[0] = "--test") then true else false

    if do_test then
        runTests()
        0
    else // run program normally

        (* read in the input file *)
        let file = argv.[0]

        let input = readProgram file


        (* try to parse what they gave us *)
        let ast_maybe = parse input do_debug


        (* try to evaluate what we parsed... or not *)
        match ast_maybe with
        | Some ast ->
            try
                let subsituted_expr, _ = expandTypeInstances ast Map.empty //get ast with no type instances
                generateSVGs subsituted_expr
                printfn "\nSVG images generated!\n"
            with
            | ex ->
                printfn "Caught exception: %s" ex.Message
            0
        | None     ->
            printfn "Invalid program."
            1
    