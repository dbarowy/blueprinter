module Evaluator

open AST
open System

(* Represents a type environment *)
type Env = Map<string,Expr>

(* prettyprint
 *   Prints out Blub expressions all nice looking.
 *   The moral equivalent of toString in Java.
 *)
let rec prettyprint (e: Expr) : string =
    match e with
    | Num n -> string n
    | EString s -> s
    | Variable v -> "Variable(" + v + ")"
    | Attribute (key, value) -> "Attribute(key: " + (prettyprint key) + ", value: " + (prettyprint value) + ")"
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
    | Assignment (lhs, rhs) ->
        "Assignment(" + (prettyprint lhs) + ", " + (prettyprint rhs) + ")"
    | Sequence es ->
        let ps = es |> List.map prettyprint
        let ss = String.Join(", ", ps)
        "Sequence(" + ss + ")"



(* evalExpr
 *   The Blueprint interpreter!
 *   Always takes in an expression and an environment, returns a string tuple
 *   of <filename>, <svg string>, <env>
 *
 *   Blueprint does not statically check data types; it does
 *   perform dynamic checks in some places.
 *)
let rec eval (e: Expr)(env: Env) : string option * Env =
    match e with
    | Num x -> Some (string x), env
    | EString x -> Some (x), env
    | Variable v ->
        if Map.containsKey v env then
            let value = env[v]
            eval value env
        else
            printfn "Undefined variable."
            exit 1
    | Furniture (_)-> 
        // type check attrs
        failwith "not implemented yet."
        e, env
    | Attribute(_, _) ->
        // type check key and value
        failwith "not implemented yet."
        e, env
    | Room (attrs, children)->
        // Type check attrs and children
        failwith "not implemented yet."
        e, env
    | Level (attrs, children)->
        // Type check attrs and children
        failwith "not implemented yet."
        None, env
    | TypeDef (pars, children) -> 
        // Type check pars, attrs, and children
        failwith "not implemented yet."
        e, env
    | Assignment (lhs, rhs) ->
        match rhs with
        | TypeDef (_, _) ->
            ()
        | _ -> 
            printfn "Right hand side of an assignment must be a type definition."
            exit 1

        match lhs with
        | Variable v ->
            let envNew = env.Add (v, rhs)
            None, envNew
        | _ ->
            printfn "Left hand side of an assignment must be a variable."
            exit 1
    | Sequence es ->
        failwith "Should not reach here. Sequences can't be nested."
        exit 1



(* evalBlueprint
 *   The Blueprint interpreter!
 *   Always takes in an expression and an environment, returns nothing,
 *   and generates the SVG images
 *
 *   Blueprint does not statically check data types; it does
 *   perform dynamic checks in some places.
 *)
let rec evalBlueprint (e: Expr)(env: Env) : unit =
    match e with
    | Sequence es ->
        List.map (fun x -> 
            let filepath, svgStr = evalExpr e env
            match filepath with
            | Some path -> 
                use file = IO.File.CreateText(path)
                file.Write(svgStr)
                0
            | None ->
                failwith "error generating SVG. This should not be hit."
                exit 1
        ) es |> ignore


    | _ ->
        failwith "Sequence must be of Levels or Type Definitions."
        exit 1
