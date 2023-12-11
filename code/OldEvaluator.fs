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
let rec getArgsPairs (pars: Expr list)(args: Expr list) = 
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
        | A::As -> (x, A)::(getArgsPairs Ps As)
    | _ ->
        failwith "parameter must be variable."
        exit 11

    

(* subsituteArgsHelper
 *   Recursively replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec subsituteArgsHelper (expr: Expr)(env: Args) : Expr =
    match expr with
    | Num(_) -> expr
    | EString(_) -> expr
    | Variable(v) -> 
        if Map.containsKey v env then
            let value = env[v]
            value
        else
            printfn "Undefined parameter."
            exit 1
    | Property(key, value) ->
        Property((subsituteArgsHelper key env), (subsituteArgsHelper value env))
    | Furniture(attrs) ->
        Furniture(List.map (fun attr -> subsituteArgsHelper attr env) attrs)
    | Room(attrs, children) ->
        let newAttrs = List.map (fun attr -> subsituteArgsHelper attr env) attrs
        let newChildren = List.map (fun child -> subsituteArgsHelper child env) children
        Room(newAttrs, newChildren)
    | Level(attrs, children) ->
        let newAttrs = List.map (fun attr -> subsituteArgsHelper attr env) attrs
        let newChildren = List.map (fun child -> subsituteArgsHelper child env) children
        Level(newAttrs, newChildren)
    | TypeDef(pars, children) ->
        let newPars = List.map (fun par -> subsituteArgsHelper par env) pars
        let newChildren = List.map (fun child -> subsituteArgsHelper child env) children
        TypeDef(newPars, newChildren)    
    | TypeInstance(var, args) ->
        let newVar = subsituteArgsHelper var env
        let newArgs = List.map (fun arg -> subsituteArgsHelper arg env) args
        TypeInstance(newVar, newArgs)
    | Assignment(left, right) ->
        Assignment(subsituteArgsHelper left env, subsituteArgsHelper right env)
    | Sequence(exprs) ->
        Sequence(List.map (fun e -> subsituteArgsHelper e env) exprs)




(* subsituteArgs
 *   Replaces a type def instance with its children and arguments subsituted
 *)
let rec subsituteArgs (def: TypeDef)(args: Expr list) : Expr list =
    let pars, children = def
    let env: Args = getArgsPairs pars args |> Map.ofList
    match (subsituteArgsHelper (Sequence children) env) with
    | Sequence(newChildren) -> newChildren
    | _ ->
        failwith "This should never be touched."
        exit 1
    


(* subsituteTypeDefs
 *   Replaces all type def instances with the type def constructor and correct arguments and children objects
 *)
let rec subsituteTypeDefs (e: Expr)(env: Env) : Expr * Env =
    match e with
    | Num _ -> e, env
    | EString _ -> e, env
    | Variable v -> e, env
        // if Map.containsKey v env then
        //     let value = env[v]
        //     value, env
        // else
        //     printfn "Undefined variable."
        //     exit 1
    | Property(key, value) ->
        let newKey, _ = subsituteTypeDefs key env
        let newValue, _ = subsituteTypeDefs value env
        Property (newKey, newValue), env
    | Furniture (attrs)-> 
        Furniture(List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) attrs), env
    | Room (attrs, children)->
        let newAttrs = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) attrs)
        let newChildren = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) children)
        // Type check attrs and children
        Room(newAttrs, newChildren), env
    | Level (attrs, children)->
        let newAttrs = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) attrs)
        let newChildren = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) children)
        // Type check attrs and children
        Level(newAttrs, newChildren), env
    | TypeDef (pars, children) -> 
        let newAttrs = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) pars)
        let newChildren = (List.map (fun attr -> 
            let newExpr, _ = subsituteTypeDefs attr env
            newExpr
        ) children)
        // Type check attrs and children
        TypeDef(newAttrs, newChildren), env
    | TypeInstance(var, pars) ->
        failwith "Should never get here, should be in Sequence, Level, or Room."
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
        match es with
        | [] ->
            printfn "Empty sequence not allowed."
            exit 1
        | [e] -> subsituteTypeDefs e env
        | e::es2 ->
            match e with
            | Assignment (_, _) ->
                let _, env1 = subsituteTypeDefs e env
                let s = Sequence es2
                subsituteTypeDefs s env1
            | _ ->
                failwith "Sequence must be of type definition assignments."
                exit 1