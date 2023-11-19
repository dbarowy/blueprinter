module Parser

open Combinator
open AST

(* declare expression parser so we can use it recursively *)
let pexpr,pexprImpl = recparser()

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


(* pfurniture
 *   Parses a furniture object. Furniture is a tuple of the name and the image path
 *)
let pfurniture: Parser<char> = pletter <|> pdigit <!> "pvarchar"
let pvarchar: Parser<char> = pletter <|> pdigit <!> "pvarchar"
let pvar: Parser<Expr> = pseq pletter (pmany0 pvarchar |>> stringify)
                           (fun (c: char, s: string) -> (string c) + s)
                           |>> Variable <!> "pvar"


(* passign
 *   Parses an assignment, e.g.,
 *   x := 2
 *)
let passign = pseq (pleft (pad pvar) (pad (pstr ":="))) (pad pexpr) Assignment <!> "passign"

(* pplus
 *   Parses an addition operation.  Has no real notion
 *   of precedence or associativity, so watch out!
 *)
let pplus = pseq (pleft (pad (pnum <|> pvar)) (pad (pchar '+'))) (pad pexpr) Plus <!> "pplus"

(* print
 *   Parses a print expression, e.g.,
 *   print "hi"
 *)
let pprint = pright (pad (pstr "print ")) pexpr |>> Print <!> "pprint"

(* pparens
 *   Parses an expression surrounded by parens.  Discards
 *   parens entirely (there is no Parens AST type), but it
 *   does force the parser to nest expressions correctly in
 *   some cases.  E.g.,
 *   1 + (x + 1)
 *)
let pparens = pbetween (pad (pchar '(')) pexpr (pad (pchar ')')) <!> "pparens"

(* pexpr
 *   Parses an arbitrary expression.  In general, tries
 *   to parse the most distinguisable/most complex thing
 *   first.
 *)
pexprImpl := pprint <|> pparens <|> pplus <|> passign <|> pstring <|> pnum <|> pvar <!> "pexpr"

(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft (pad pexpr) pws0) |>> Sequence <!> "pexprs"

(* grammar
 *  Top level parser definition.  Call this one
 *  if you want a Blub parser.
 *)
let grammar = pleft pexprs (peof <|> pcomment) <!> "grammar"

(* parse
 *  User-friendly function that calls the Blub parser
 *  and returns an optional Expr.
 *)
let parse (input: string)(do_debug: bool) : Expr option =
    let i = (if do_debug then debug else prepare) input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_)   -> None