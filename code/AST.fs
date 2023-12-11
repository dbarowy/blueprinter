module AST


type Expr =
| Num of int
| EString of string
| Variable of string
| Property of key: Expr * value: Expr
| Furniture of attrs: Expr list
| Room of attrs: Expr list * children: Expr list
| Level of attrs: Expr list * children: Expr list
| TypeDef of TypeDef
| TypeInstance of var: Expr * args: Expr list
| Assignment of Expr * Expr
| Sequence of Expr list

and TypeDef = Expr list *  Expr list