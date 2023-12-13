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
    | TypeInstance (var, pars) ->
        let prettyPars = pars |> List.map prettyprint
        let joinedPars = String.Join(", ", prettyPars)
        "TypeInstance(" + (prettyprint var) + ", " + joinedPars + ")"
    | Assignment (lhs, rhs) ->
        "Assignment(" + (prettyprint lhs) + ", " + (prettyprint rhs) + ")"
    | Sequence es ->
        let ps = es |> List.map prettyprint
        let ss = String.Join(", ", ps)
        "Sequence(" + ss + ")"



(* getArgsMap
 *   get map of parameters to arguments
 *)
let rec getArgsEnv (pars: Expr list)(args: Expr list) = 
    match pars with 
    | [] -> 
        match args with
        | [] -> []
        | _ ->
            failwith "Incorrect number of arguments specified."
            exit 1
    | Variable(x)::Ps ->
        match args with
        | [] ->
            failwith "Incorrect number of arguments specified."
            exit 1
        | A::As -> (x, A)::(getArgsEnv Ps As)
    | _ ->
        failwith "parameter must be variable."
        exit 11

    

(* subsituteArgsHelper
 *   Recursively replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec subsituteArgs (expr: Expr)(args: Args)(env: Env) : Expr =
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
        Property((subsituteArgs key args env), (subsituteArgs value args env))
    | Furniture(attrs) ->
        Furniture(List.map (fun attr -> subsituteArgs attr args env) attrs)
    | Room(attrs, children) ->
        let newAttrs = List.map (fun attr -> subsituteArgs attr args env) attrs
        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, newArgs) ->
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        let subsitutedArgs = List.map (fun arg -> subsituteArgs arg args env) newArgs
                        expandTypeInstance value subsitutedArgs env
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    failwith "Type instance must be a variable."
            | _ -> acc @ [subsituteArgs child args env]
        ) [] children)
        Room(newAttrs, newChildren)
    | Level(attrs, children) ->
        failwith "Cannot have nested levels."
        exit 1
    | TypeDef(pars, children) ->
        failwith "Cannot have nested type definitions."
        exit 1
    | TypeInstance(var, args) ->
        failwith "type instance should not be hit here."
        exit 1
    | Assignment(left, right) ->
        failwith "Cannot have nested type definitions."
        exit 1
    | Sequence(exprs) ->
        Sequence(List.map (fun e -> subsituteArgs e args env) exprs)




(* subsituteArgs
 *   Replaces a type def instance with its children and arguments subsituted
 *)
and expandTypeInstance (def: TypeDef)(args: Expr list)(env: Env) : Expr list =
    let pars, children = def
    let args: Args = getArgsEnv pars args |> Map.ofList

    match (subsituteArgs (Sequence children) args env) with
    | Sequence(newChildren) -> newChildren
    | _ ->
        failwith "This should never be touched."
        exit 1
    


(* expandTypeInstances
 *   Replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec expandTypeInstances (e: Expr)(env: Env) : Expr * Env =
    match e with
    | Num _ -> e, env
    | EString _ -> e, env
    | Variable v -> e, env
    | Property(_, _) -> e, env
    | Furniture (_)-> e, env
    | Room (attrs, children)->
        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, args) ->
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        expandTypeInstance value args env
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    failwith "Type instance must be a variable."
            | _ -> acc @ [child]
        ) [] children)
        Room(attrs, newChildren), env
    | Level (attrs, children)->
        let newChildren = (List.fold (fun acc child -> 
            match child with 
            | TypeInstance(var, args) ->
                match var with
                | Variable(v) ->
                    if Map.containsKey v env then
                        let value = env[v]
                        expandTypeInstance value args env
                    else
                        printfn "Undefined type."
                        exit 1
                | _ ->
                    failwith "Type instance must be a variable."
            | _ -> acc @ [child]
        ) [] children)
        Level(attrs, newChildren), env
    | TypeDef (_, _) -> e, env
    | TypeInstance(_, _) -> 
        failwith "type instance should not be hit here."
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
            | TypeInstance(_, _) ->
                failwith "Type instance must be used within a level."
                exit 1
            | _ ->
                failwith "Sequence must be of type definition and levels."
                exit 1
        ) ([], Map.empty) es)

        Sequence(List.rev newExprs), newEnv