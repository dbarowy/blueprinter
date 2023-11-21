module Parser

open Combinator
open AST

(* declare expression parser so we can use it recursively *)
let pexpr,pexprImpl = recparser()

(* my_ws
 *   Let's consider any non-newline whitespace or
 *   a comment to be whitespace
 *)
let my_ws = (pwsNoNL0 |>> (fun _ -> true))


(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween my_ws p my_ws


(* pnum
 *   Parses a number.
 *)
let pnum: Parser<Expr> = pmany1 pdigit
                         |>> (fun ds ->
                                let s = stringify ds
                                let n = int s
                                let e = Num n
                                e
                             ) <!> "pnum"

(* pstring
 *   Parses a string literal.  String literals cannot
 *   contain double quotes.
 *)
let pnotquot: Parser<char> = psat (fun c -> c <> '"') <!> "pnotquot"
let pstring: Parser<Expr> = pbetween (pchar '"') (pmany1 pnotquot) (pchar '"')
                            |>> (fun cs ->
                                   let s = stringify cs
                                   let e = EString s
                                   e
                                ) <!> "pstring"

(* pvar
 *   Parses a variable.  Variable names are at least one
 *   character long, starting with a letter, followed by
 *   any combination of letters or numbers.
 *)
let pvarchar: Parser<char> = pletter <|> pdigit <!> "pvarchar"
let pvar: Parser<Expr> = pseq pletter (pmany0 pvarchar |>> stringify)
                           (fun (c: char, s: string) -> (string c) + s)
                           |>> Variable <!> "pvar"

(* pattribute
 *   Parses an attribute.
 *)
let pattribute: Parser<Expr> = 
   let pleft = pleft pvar (pchar '=')
   pseq pleft (pstring <|> pnum <|> pvar) (fun (key, value) -> Attribute(key, value)) <!> "pattribute"

(* pattributes
 *   Helper parser for list of attributes.
 *)
let pattributeAdditional = pright (pstr ",") pattribute
let pattributes: Parser<Expr list> = 
   let emptyList = []
   (pseq pattribute (pmany0 pattributeAdditional) (fun (attr, attrs) -> attr::attrs)) <|> (presult emptyList) <!> "pattributes"

(* pfurniture
 *   Parses a furniture object. Furniture is a tuple of the name and the image path
 *)
let pfurniture: Parser<Expr> = 
   (pbetween (pstr "Furniture(") pattributes (pstr ")")) |>> Furniture <!> "pfurniture"


(* pchildren
 *   Helper parser for children objects.
 *)
let pchildren: Parser<Expr list> = 
   (pmany0 (pleft (pexpr) pnl ))  <!> "pchildren"


(* proom
 *   Parses a room object.
 *)
let proomInside: Parser<Expr> = 
   pseq (pleft pattributes (pstr ")[\n")) pchildren (fun (attrs, children) -> Room(attrs, children)) <!> "proomInside"
let proom: Parser<Expr> = 
   pbetween (pstr "Room(") proomInside (pstr "]") <!> "proom"


(* plevel
 *   Parses a level object.
 *)
let plevelInside: Parser<Expr> = 
   pseq (pleft pattributes (pstr ")[\n")) pchildren (fun (attrs, children) -> Room(attrs, children)) <!> "plevelInside"
let plevel: Parser<Expr> = 
   pbetween (pstr "Level(") plevelInside (pstr "]") <!> "plevel"


(* ptypedef
 *   Parses a level object.
 *)
let ppars: Parser<Expr list> = 
   pmany0 pvar <!> "ppars"
let ptypedef: Parser<Expr> = 
   pseq (pbetween (pstr " (") ppars (pstr ")[\n")) (pleft pchildren (pchar ']')) (fun (pars, children) -> TypeDef(pars, children)) <!> "ptypedef"


(* passign
 *   Parses an assignment, e.g.,
 *   type MiniGolf (length, width):
         ...
 *)
let pdecleration: Parser<Expr> =
   pright (pstr "type ") pvar
let passign = pseq (pdecleration) (ptypedef) Assignment <!> "passign"

(* pexpr
 *   Parses an arbitrary expression.  In general, tries
 *   to parse the most distinguisable/most complex thing
 *   first.
 *)
pexprImpl := passign <|> ptypedef <|> plevel <|> proom <|> pfurniture <|> pattribute <|> pvar <|> pstring <|> pnum <!> "pexpr"
(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft pexpr pnl) |>> Sequence <!> "pexprs"

(* grammar
 *  Top level parser definition.  Call this one
 *  if you want a Blub parser.
 *)
let grammar = pleft pexprs peof <!> "grammar"

(* parse
 *  User-friendly function that calls the Blub parser
 *  and returns an optional Expr.
 *)
let parse (input: string)(do_debug: bool) : Expr option =
    let i = (if do_debug then debug else prepare) input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_)   -> None