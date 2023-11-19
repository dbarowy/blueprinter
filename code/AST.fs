module AST


type Expr =
| Num of int
| EString of string
| Variable of string
| Furniture of name: string * imagePath: string
| Room of attrs: Expr list * children: Expr list
| Level of attrs: Expr list * children: Expr list
| TypeDef of baseType: Expr * pars: Expr list * attrs: Expr list * children: Expr list
| Assignment of Expr * Expr
| Sequence of Expr list
