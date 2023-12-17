module Tests

open Parser
open Evaluator
open AST

let testParser(): bool =
    let input = """
    Level("filepath" = "example.svg", "width" = 1200, "height" = 700) {
        Room("x" = 0, "y" = 350, "height"=350, "width"=200, "name" = "Example Room") {}
    }
    """
    
    let expectedResult =  
        (Some
        (Sequence
            [Level
                ([Property (EString "filepath", EString "example.svg");
                Property (EString "width", Num 1200);
                Property (EString "height", Num 700)],
                [Room
                    ([Property (EString "x", Num 0); Property (EString "y", Num 350);
                    Property (EString "height", Num 350);
                    Property (EString "width", Num 200);
                    Property (EString "name", EString "Example Room")], [])])]))


    expectedResult = parse input false


let testInterpreter(): bool =
    let expr =  
            (Level
                    ([Property (EString "filepath", EString "example.svg");
                    Property (EString "width", Num 1200);
                    Property (EString "height", Num 700)],
                    [Room
                        ([Property (EString "x", Num 0); Property (EString "y", Num 350);
                        Property (EString "height", Num 350);
                        Property (EString "width", Num 200);
                        Property (EString "name", EString "Example Room")], [])]))
    
    let expected_result = "<svg width=\"1220\" height=\"720\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n\t<rect x=\"10\" y=\"10\" width=\"1200\" height=\"700\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n\t<rect x=\"10\" y=\"360\" width=\"200\" height=\"350\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n\t<text x=\"110\" y=\"535\" font-family=\"Arial\" font-size=\"20\" fill=\"black\" text-anchor=\"middle\" dominant-baseline=\"middle\" >Example Room</text>\n</svg>\n"
    let actual_result = eval expr 10 10 

    expected_result = actual_result 

let testExpandTypeInstances(): bool = 
    let ast =  
        (Sequence
        [Assignment
            (Variable "ExampleRoom",
            TypeDef
                ([],
                [Room
                    ([Property (EString "x", Num 0); Property (EString "y", Num 0);
                    Property (EString "height", Num 350);
                    Property (EString "width", Num 200);
                    Property (EString "name", EString "Example Room")], [])]));
        Level
            ([Property (EString "filepath", EString "example.svg");
            Property (EString "width", Num 1200);
            Property (EString "height", Num 700)],
            [TypeInstance
                (Variable "ExampleRoom", Property (EString "x", Num 0),
                Property (EString "y", Num 350), [])])])
    
    let expected_result = 
        (Sequence
        [Level
            ([Property (EString "filepath", EString "example.svg");
            Property (EString "width", Num 1200);
            Property (EString "height", Num 700)],
            [Room
                ([Property (EString "x", Num 0); Property (EString "y", Num 350);
                Property (EString "height", Num 350);
                Property (EString "width", Num 200);
                Property (EString "name", EString "Example Room")], [])])])
    
    let subsituted_expr, _ =  expandTypeInstances ast Map.empty

    expected_result = subsituted_expr

let runTests(): unit =
    let parserTestResult = testParser()
    let InterpreterTestResult = testInterpreter()
    let expandTypeInstancesTestResult = testExpandTypeInstances()
    
    if parserTestResult then
        printfn ""
    else
        printfn "Parser test failed"

    if InterpreterTestResult then
        printfn ""
    else
        printfn "Interpreter test failed"
    
    if expandTypeInstancesTestResult then
        printfn ""
    else
        printfn "ExpandTypeInstances test failed"

    
    if parserTestResult && InterpreterTestResult && expandTypeInstancesTestResult then
        printfn "ALL TESTS PASSED!\n"
    else
        printfn "\n\nSOME TESTS FAILED. SEE ABOVE.\n"