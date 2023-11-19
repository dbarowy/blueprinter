module Evaluator

open AST
open System

(* Represents a variable environment *)
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
    | Furniture (name, imagePath)-> "Variable(" + name + ", " + imagePath + ")"
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
    | TypeDef (baseType, pars, attrs, children) ->
        let prettyBaseType = prettyprint baseType
        let prettyPars = pars |> List.map prettyprint
        let joinedPars = String.Join(", ", prettyPars)
        let prettyAttrs = attrs |> List.map prettyprint
        let joinedAttrs = String.Join(", ", prettyAttrs)
        let prettyChildren = children |> List.map prettyprint
        let joinedChildren = String.Join(", ", prettyChildren)
        "TypeDef(baseType: " + prettyBaseType + ", pars: " + joinedPars + ", attrs: " + joinedAttrs + ", children: " + joinedChildren + ")" 
    | Assignment (lhs, rhs) ->
        "Assignment(" + (prettyprint lhs) + ", " + (prettyprint rhs) + ")"
    | Sequence es ->
        let ps = es |> List.map prettyprint
        let ss = String.Join(", ", ps)
        "Sequence(" + ss + ")"

(* eval
 *   The Blub interpreter!
 *   Always takes in an expression and an environment, and
 *   always returns an expression and an updated environment.
 *
 *   Blub does not statically check data types; it does
 *   perform dynamic checks in some places.
 *   However, Blub is "closed" under all Blub operations,
 *   meaning that a Blub expression will never return
 *   something that isn't a Blub expression.
 *)
let rec eval (e: Expr)(env: Env) : Expr * Env =
    match e with
    | Num _ -> e, env
    | EString _ -> e, env
    | Variable v ->
        if Map.containsKey v env then
            let value = env[v]
            value, env
        else
            printfn "Undefined variable."
            exit 1
    | Assignment (lhs, rhs) ->
        match lhs with
        | Variable v ->
            let rhsr, env1 = eval rhs env
            let env2 = env1.Add (v, rhsr)
            rhsr, env2
        | _ ->
            printfn "Left hand side of an assignment must be a variable."
            exit 1
    | Plus (lhs, rhs) ->
        let lhsr, env1 = eval lhs env
        let rhsr, env2 = eval rhs env1
        match lhsr, rhsr with
        | Num n1, Num n2 -> Num (n1 + n2), env2
        | _ ->
            printfn "Invalid operation. Plus requires numeric operands."
            exit 1
    | Print e ->
        let er, env1 = eval e env
        printfn "%s" (prettyprint er)
        er, env1
    | Sequence es ->
        match es with
        | [] ->
            printfn "Empty sequence not allowed."
            exit 1
        | [e] -> eval e env
        | e::es2 ->
            let _, env1 = eval e env
            let s = Sequence es2
            eval s env1