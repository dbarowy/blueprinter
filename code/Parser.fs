module Parser

open Combinator
open AST

(* declare expression parser so we can use it recursively *)
let pexpr,pexprImpl = recparser()

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween pws0 p pws0


(* space p1 p2 f
 *   Parses p1 and p2, with optional whitespace in between.
 *)
let space p1 p2 f = 
   pseq (pleft p1 pws0) p2 f


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
let pstring: Parser<Expr> = (pbetween (pchar '"') (pmany1 pnotquot) (pchar '"'))
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

(* px
 *   Parses a "x" property
 *)
let px: Parser<Expr> = 
   let pleft = pleft (pad (pstr "\"x\"")) (pchar '=')
   pseq pleft (pad pnum) (fun (key, value) -> Property(EString(key), value)) <!> "px"

(* py
 *   Parses a "y" property
 *)
let py: Parser<Expr> = 
   let pleft = pleft (pad (pstr "\"y\"")) (pchar '=')
   pseq pleft (pad pnum) (fun (key, value) -> Property(EString(key), value)) <!> "py"


(* pproperty
 *   Parses an property of an object.
 *)
let pproperty: Parser<Expr> = 
   let pleft = pleft (pad pstring) (pchar '=')
   pseq pleft (pad (pstring <|> pnum <|> pvar)) (fun (key, value) -> Property(key, value)) <!> "pproperty"

(* pproperties
 *   Helper parser for list of properties.
 *)
let ppropertyAdditional = pright (pstr ",") pproperty
let pproperties: Parser<Expr list> = 
   (pseq pproperty (pmany0 ppropertyAdditional) (fun (attr, attrs) -> attr::attrs)) <!> "pproperties"


(* pfurniture
 *   Parses a furniture object. Furniture is a tuple of the name and the image path
 *)
let pfurniture: Parser<Expr> = 
   (pbetween (pstr "Furniture(") pproperties (pstr ")")) |>> Furniture <!> "pfurniture"


(* pchildrenRoom
 *   Helper parser for room children objects.
 *)
let rec pchildrenRoom = 
   pmany0 (pad pexpr)  <!> "pchildrenRoom"


(* proom
 *   Parses a room object.
 *)
let proom = 
   pbetween (pstr "Room(") (pseq (pleft pproperties (space (pchar ')') (pchar '{') (fun (a, b) -> a))) pchildrenRoom (fun (attrs, children) -> Room(attrs, children))) (pstr "}") <!> "proom"


(* pchildrenLevel
 *   Helper parser for level children objects.
 *)
let rec pchildrenLevel = 
   pmany0 (pad pexpr )  <!> "pchildrenLevel"

(* plevel
 *   Parses a level object.
 *)
let plevelInside: Parser<Expr> = 
   pseq (pleft pproperties (space (pchar ')') (pchar '{') (fun (a, b) -> a))) pchildrenLevel (fun (attrs, children) -> Level(attrs, children)) <!> "plevelInside"
let plevel =
   pbetween (pstr "Level(") plevelInside (pstr "}") <!> "plevel"


(* ptypedef
 *   Parses a level object.
 *)
let pparAdditional = pright (pstr ",") (pad pvar) <!> "pparAdditional"
let ppars: Parser<Expr list> = 
   let emptyList = []
   (pseq (pad pvar) (pmany0 pparAdditional) (fun (attr, attrs) -> attr::attrs)) <|> (presult emptyList) <!> "ppars"
let ptypedef: Parser<Expr> = 
   pseq (pbetween (pstr "(") ppars (space (pchar ')') (pchar '{') (fun (a, b) -> a))) (pleft pchildrenLevel (pchar '}')) (fun (pars, children) -> TypeDef(pars, children)) <!> "ptypedef"

(* pinstance
 *   Parses an instance, e.g.,
 *   MiniGolf (length, width){...}
 *)
let pargAdditional = pright (pstr ",") (pad (pstring <|> pnum <|> pvar)) <!> "pargAdditional"
let pargs = 
   (pseq (pleft px (pchar ',')) (pseq py (pmany0 pargAdditional) (fun (yattr,args) -> yattr, args)) (fun (xattr, (yattr, args)) -> (xattr, yattr, args))) <!> "pargs"
let pinstance = 
      pseq pvar (pbetween (pstr "(") pargs (pchar ')')) (fun (var, (x, y, args)) -> TypeInstance(var, x, y, args)) <!> "pinstance"

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
pexprImpl := passign <|> plevel <|> proom <|> pfurniture <|> pinstance <|> pproperty <|> pvar <|> pstring <|> pnum <!> "pexpr"
(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pad (passign <|> pinstance <|> plevel)) |>> Sequence <!> "pexprs" // at the highest level, only type defs and levels and type instances can be made

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