module AST


type Expr =
| Num of int
| EString of string
| Variable of string
| Attribute of key: Expr * value: Expr
| Furniture of attrs: Expr list
| Room of attrs: Expr list * children: Expr list
| Level of attrs: Expr list * children: Expr list
| TypeDef of pars: Expr list * children: Expr list
| TypeInstance of var: Expr * pars: Expr list * children: Expr list
| Assignment of Expr * Expr
| Sequence of Expr list
