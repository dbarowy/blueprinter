module Evaluator

open AST
open System

(* Represents a type environment *)
type Env = Map<string,TypeDef>
type Args = Map<string, Expr>

(* prettyprint
 *   Prints out Blub expressions all nice looking.
 *   The moral equivalent of toString in Java.
 *)
let rec prettyprint (e: Expr) : string =
    match e with
    | Num n -> string n
    | EString s -> s
    | Variable v -> "Variable(" + v + ")"
    | Property (key, value) -> "Property(key: " + (prettyprint key) + ", value: " + (prettyprint value) + ")"
    | Furniture (attrs)-> 
        let prettyAttrs = attrs |> List.map prettyprint
        let joinedAttrs = String.Join(", ", prettyAttrs)
        "Furniture(" + joinedAttrs + ")"
    | Room (attrs, children) ->
        let prettyAttrs = attrs |> List.map prettyprint
        let joinedAttrs = String.Join(", ", prettyAttrs)
        let prettyChildren = children |> List.map prettyprint
        let joinedChildren = String.Join(", ", prettyChildren)
        "Room(" + "attrs: " + joinedAttrs + ", children: " + joinedChildren + ")" 
    | Level (attrs, children) ->
        let prettyAttrs = attrs |> List.map prettyprint
        let joinedAttrs = String.Join(", ", prettyAttrs)
        let prettyChildren = children |> List.map prettyprint
        let joinedChildren = String.Join(", ", prettyChildren)
        "Room(" + "attrs: " + joinedAttrs + ", children: " + joinedChildren + ")" 
    | TypeDef (pars, children) ->
        let prettyPars = pars |> List.map prettyprint
        let joinedPars = String.Join(", ", prettyPars)
        let prettyChildren = children |> List.map prettyprint
        let joinedChildren = String.Join(", ", prettyChildren)
        "TypeDef(pars: " + joinedPars + ", children: " + joinedChildren + ")" 
    | TypeInstance (var, _, _, pars) ->
        let prettyPars = pars |> List.map prettyprint
        let joinedPars = String.Join(", ", prettyPars)
        "TypeInstance(" + (prettyprint var) + ", " + joinedPars + ")"
    | Assignment (lhs, rhs) ->
        "Assignment(" + (prettyprint lhs) + ", " + (prettyprint rhs) + ")"
    | Sequence es ->
        let ps = es |> List.map prettyprint
        let ss = String.Join(", ", ps)
        "Sequence(" + ss + ")"


(* getNumFromProperty
 *   If you know an expr is a property to a Num, pulls out the num
 *   Used for x, y, width, height properties
 *)
let getNumFromProperty(num: Expr): int =
    match num with
    | Property(_, value) ->
        match value with
        | Num(x) -> x
        | _ ->
            printfn "property of x or y must be a number"
            exit 1
    | _ ->
        printfn "this should only be called with a property"
        exit 1


(* mapGetInt
 *   Get an item from a map given a string key and we know the value should be a Num
 *)
let mapGetInt(map: Args)(key: string): int = 
    if Map.containsKey key map then
        let value = map[key]
        match value with
        | Num(x) -> x
        | _ ->
            printfn "Value of %s property should only be an int." key
            exit 1
    else
       printfn "Object doesn't have attribute %s" key
       exit 1


(* mapGetString
 *   Get an item from a map given a string key and we know the value should be a EString
 *)
let mapGetString(map: Args)(key: string): string = 
    if Map.containsKey key map then
        let value = map[key]
        match value with
        | EString(s) -> s
        | _ ->
            printfn "Value of %s property should only be an string." key
            exit 1
    else
       printfn "Object doesn't have attribute %s" key
       exit 1


(* getArgsMap
 *   get map of parameters to arguments
 *)
let rec getArgsMap (pars: Expr list)(args: Expr list) = 
    match pars with 
    | [] -> 
        match args with
        | [] -> []
        | _ ->
            printfn "Incorrect number of arguments specified."
            exit 1
    | Variable(x)::Ps ->
        match args with
        | [] ->
            printfn "Incorrect number of arguments specified."
            exit 1
        | A::As -> (x, A)::(getArgsMap Ps As)
    | _ ->
        printfn "parameter must be variable."
        exit 11


(* getPropertiesMap
 *   get map of property keys to values
 *)
let rec getPropertiesMap (properties: Expr list): Args = 
    (List.map (fun property -> 
        match property with
        | Property(key, value) ->
            match key with
            | EString(s) ->
                (s, value)
            | _ ->
                printfn "Property key must be a string."
                exit 1
        | _ ->
            printfn "This function takes a list of Properties."
            exit 1
    ) properties) |> Map.ofList

    

(* subsituteArgsHelper
 *   Recursively replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec subsituteArgs (expr: Expr)(x: int)(y: int)(args: Args)(env: Env) : Expr =
    match expr with
    | Num(_) -> expr
    | EString(_) -> expr
    | Variable(v) ->
        if Map.containsKey v args then
            let value = args[v]
            value
        else
            printfn "Undefined parameter."
            exit 1
    | Property(key, value) ->
        // first handle X
        match key with
        | EString(s) ->
            // first handle X
            if s = "x" then
                match value with 
                | Num(propertyX) ->
                    Property(key, Num(propertyX + x))
                | _ ->
                    printfn "Value of x must be Num."
                    exit 1
            elif s = "y" then 
                match value with 
                | Num(propertyY) ->
                    Property(key, Num(propertyY + y))
                | _ ->
                    printfn "Value of y must be Num."
                    exit 1
            else
                Property(key, subsituteArgs value x y args env)
        | _ ->
            printfn "This will never be hit. Properties always have a EString as the key."
            exit 1

    | Furniture(attrs) ->
        Furniture(List.map (fun attr -> subsituteArgs attr x y args env) attrs)
    | Room(attrs, children) ->
        let newAttrs = List.map (fun attr -> subsituteArgs attr x y args env) attrs
        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, xNew, yNew , newArgs) ->
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        let subsitutedArgs = List.map (fun arg -> subsituteArgs arg x y args env) newArgs
                        
                        acc @ (expandTypeInstance value xNew yNew subsitutedArgs env)
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    printfn "Type instance must be a variable."
                    exit 1
            | _ -> acc @ [subsituteArgs child 0 0 args env]
        ) [] children)
        Room(newAttrs, newChildren)
    | Level(attrs, children) ->
        printfn "Cannot have nested levels."
        exit 1
    | TypeDef(pars, children) ->
        printfn "Cannot have nested type definitions."
        exit 1
    | TypeInstance(var, xNew, yNew, typeArgs) ->
        match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        
                        // get the sum of x and y and the new x and y
                        let xSum = x + getNumFromProperty(xNew)
                        let ySum = y + getNumFromProperty(yNew)

                        let newArgs = List.map (fun arg -> subsituteArgs arg x y args env) typeArgs

                        Sequence(expandTypeInstance value (Property(EString("x"), Num(xSum))) (Property(EString("y"), Num(ySum))) newArgs env)
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    printfn "Type instance must be a variable."
                    exit 1

    | Assignment(left, right) ->
        printfn "Cannot have nested type definitions."
        exit 1
    | Sequence(exprs) ->
        let newExprs = (List.fold (fun acc e-> 
            let subsitutedExpr = subsituteArgs e x y args env
            match subsitutedExpr with
            | Sequence(es) -> acc @ es
            | e -> acc @  [e]

        ) [] exprs)
        Sequence(newExprs)




(* expandTypeInstance
 *   Replaces a type def instance with its children and arguments subsituted
 *)
and expandTypeInstance (def: TypeDef)(x: Expr)(y: Expr)(args: Expr list)(env: Env) : Expr list =
    let pars, children = def
    let args: Args = getArgsMap pars args |> Map.ofList

    // get relative x and y
    let xVal = getNumFromProperty x
    let yVal = getNumFromProperty y

    match (subsituteArgs (Sequence children) xVal yVal args env) with
    | Sequence(newChildren) -> newChildren
    | _ ->
        printfn "This should never be touched."
        exit 1
    


(* expandTypeInstances
 *   Replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec expandTypeInstances (e: Expr)(env: Env) : Expr * Env =
    match e with
    | Num _ -> e, env
    | EString _ -> e, env
    | Variable v -> 
        printfn "Undefined variable."
        exit 1
    | Property(key, value) ->
        // Check no undefined variables
        expandTypeInstances key env |> ignore
        expandTypeInstances value env |> ignore
        e, env
    | Furniture (attrs)-> 
        List.map (fun attr -> expandTypeInstances attr env) attrs |> ignore
        e, env
    | Room (attrs, children)->
        List.map (fun attr -> expandTypeInstances attr env) attrs |> ignore
        
        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, x, y, args) ->
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        acc @ (expandTypeInstance value x y args env)
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    printfn "Type instance must be a variable."
                    exit 1
            | _ -> 
                let childExpandedExpr, _ = expandTypeInstances child env
                acc @ [childExpandedExpr]
        ) [] children)
        Room(attrs, newChildren), env
    | Level (attrs, children) ->
        List.map (fun attr -> expandTypeInstances attr env) attrs |> ignore

        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, x, y, args) ->
                List.map (fun arg -> expandTypeInstances arg env) args |> ignore
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        acc @ (expandTypeInstance value x y args env)
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    printfn "Type instance must be a variable."
                    exit 1
            | _ -> 
                let childExpandedExpr, _ = expandTypeInstances child env
                acc @ [childExpandedExpr]
        ) [] children)
        Level(attrs, newChildren), env
    | TypeDef (pars, children) -> 
        e, env
    | TypeInstance(_, _, _, _) -> 
        printfn "type instance should not be hit here."
        exit 1
    | Assignment (lhs, rhs) ->
        match rhs with
        | TypeDef (args, children) ->
            match lhs with
            | Variable v ->
                let envNew = env.Add (v, (args, children))
                rhs, envNew
            | _ ->
                printfn "Left hand side of an assignment must be a variable."
                exit 1
        | _ -> 
            printfn "Right hand side of an assignment must be a type definition."
            exit 1
    | Sequence es ->
        let newExprs, newEnv = (List.fold (fun (eAcc, envAcc) expr -> 
            match expr with
            | Assignment (_, _) ->
                let _, newEnv = expandTypeInstances expr envAcc
                eAcc, newEnv         
            | Level(_, _) ->
                let newExpr, _ = expandTypeInstances expr envAcc
                newExpr::eAcc, envAcc
            | TypeInstance(_, _, _, _) ->
                printfn "Type instance must be used within a level."
                exit 1
            | _ ->
                printfn "Sequence must be of type definition and levels."
                exit 1
        ) ([], Map.empty) es)

        Sequence(List.rev newExprs), newEnv



(* evalLevel
 *   Generates the string SVG for the level
 *)
let rec eval (expr: Expr)(x: int)(y: int) : string =
    match expr with
    | Level(attrs, children) ->
        // Get heigth and width properties
        let properties = getPropertiesMap(attrs)
        if Map.containsKey "width" properties then
            let widthExpr = properties["width"]
            match widthExpr with
            | Num(width) ->
                if Map.containsKey "height" properties then
                    let heightExpr = properties["height"]
                    match heightExpr with
                    | Num(height) ->
                        "<svg width=\"" + string (width + (2 * x)) + "\" height=\"" + string (height + (2 * y)) + "\"" +
                        " xmlns=\"http://www.w3.org/2000/svg\"" +
                        " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
                        "\t<rect x=\"" + string x + "\" y=\"" + string y + "\" width=\"" + string width + "\" height=\"" + string height + "\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n"
                        + (eval (Sequence(children)) x y) + "</svg>\n"
                    | _ ->
                        printfn "Width must be a num."
                        exit 1     

                else
                    printfn "height not defined."
                    exit 1             
            | _ ->
                printfn "Width must be a num."
                exit 1
        
            
        else
            printfn "width not defined."
            exit 1
    | Room(attrs, children) ->
        let attrsMap = getPropertiesMap attrs
        let roomX = mapGetInt attrsMap "x"
        let roomY = mapGetInt attrsMap "y"
        let roomWidth = mapGetInt attrsMap "width"
        let roomHeight = mapGetInt attrsMap "height"
        
        if Map.containsKey "name" attrsMap then
            let roomName = mapGetString attrsMap "name"
            let textX = x + roomX + (roomWidth / 2)
            let textY = y + roomY + (roomHeight / 2)      
            "\t<rect x=\"" + string (x + roomX) + "\" y=\"" + string (y + roomY) + "\" width=\"" + string roomWidth + "\" height=\"" + string roomHeight + "\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n"
            + "\t<text x=\"" + string (textX) + "\" y=\"" + string (textY) + "\" font-family=\"Arial\" font-size=\"20\" fill=\"black\" text-anchor=\"middle\" dominant-baseline=\"middle\" >" + roomName + "</text>\n"
            + (eval (Sequence(children)) (x + roomX) (y + roomY))
        else
            "\t<rect x=\"" + string (x + roomX) + "\" y=\"" + string (y + roomY) + "\" width=\"" + string roomWidth + "\" height=\"" + string roomHeight + "\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n"
            + (eval (Sequence(children)) (x + roomX) (y + roomY))       

    | Furniture(attrs) ->
        let attrsMap = getPropertiesMap attrs
        let furnitureX = mapGetInt attrsMap "x"
        let furnitureY = mapGetInt attrsMap "y"
        let furnitureWidth = mapGetInt attrsMap "width"
        let furnitureHeight = mapGetInt attrsMap "height"

        if Map.containsKey "name" attrsMap then
            let furnitureName = mapGetString attrsMap "name"
            let textX = x + furnitureX + (furnitureWidth / 2)
            let textY = y + furnitureY + (furnitureHeight / 2)

            "\t<rect x=\"" + string (x + furnitureX) + "\" y=\"" + string (y + furnitureY) + "\" width=\"" + string furnitureWidth + "\" height=\"" + string furnitureHeight + "\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n"
            + "\t<text x=\"" + string (textX) + "\" y=\"" + string (textY) + "\" font-family=\"Arial\" font-size=\"10\" fill=\"black\" text-anchor=\"middle\" dominant-baseline=\"middle\" >" + furnitureName + "</text>\n"
        else
            "\t<rect x=\"" + string (x + furnitureX) + "\" y=\"" + string (y + furnitureY) + "\" width=\"" + string furnitureWidth + "\" height=\"" + string furnitureHeight + "\" fill=\"none\" stroke=\"blue\" stroke-width=\"2\" />\n"

    | Sequence(es) ->
        String.concat "" (List.map (fun e -> eval e x y) es)
    | _ ->
        printfn "%A" expr
        printfn "Invalid AST submitted to eval."
        exit 1



(* eval
 *   Generates an SVG file for every level given a sequence, sequence should only consist of level
 *)
let rec generateSVGs (e: Expr) : unit =
    match e with
    | Sequence(levels) ->
        (List.map (fun level -> 
            match level with 
            | Level(attrs, children) ->
                let properties = getPropertiesMap(attrs)
                if Map.containsKey "filepath" properties then
                        let value = properties["filepath"]
                        match value with
                        | EString(path) ->
                            use writer = new IO.StreamWriter(path)
                            writer.Write(eval level 10 10)
                        | _ ->
                            printfn "filepath must be a string"
                            exit 1
                        
                    else
                        printfn "filepath not defined."
                        exit 1
               
            | _ ->
                printfn "Sequence in eval must be of levels."
                exit 1
        ) levels) |> ignore
    | _ ->
        printfn "eval should only be called with a sequence."
        exit 1