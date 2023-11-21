open Parser
open Evaluator
open AST
open System.IO

[<EntryPoint>]
let main argv : int =
    (* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> [debug]"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    let input = File.ReadAllText file

    (* does the user want parser debugging turned on? *)
    let do_debug = if argv.Length = 2 then true else false
    // let ast = Sequence([Assignment(Variable("x"), TypeDef([], [Room([Attribute(EString("id"), EString("room"))], [])])); Assignment(Variable("y"), TypeDef([], [Room([Attribute(EString("id"), EString("room2"))], [])]))])
    // let expr, env = eval ast Map.empty
    // printfn "%A" env
    (* try to parse what they gave us *)
    let ast_maybe = parse input do_debug

    (* try to evaluate what we parsed... or not *)
    match ast_maybe with
    | Some ast ->
        let _, env = eval ast Map.empty
        printfn "%A" env
        0
    | None     ->
        printfn "Invalid program."
        1
    