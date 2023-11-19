(*
  A simple combinator-style parsing library for F#.

  Inspired by the Hutton & Meijer paper as well as the FParsec
  combinator library.  Other than being much smaller, this
  library trades away performance for simplicity.  If you need
  a fast library, look at FParsec.

  Version: 1.9 (2023-10-30)
*)

module Combinator

open System
open System.Text.RegularExpressions

/// <summary>
/// A 3-tuple representing a "rich string" that the parser needs for normal operation.
/// First element: the input string
/// Second element: the current position in the parse
/// Third element: a boolean which is true if debugging is enabled
/// </summary>
type Input = string * int * bool

/// <summary>
/// Use this to prepare a rich string (an <c>Input</c>) for normal (non-debug)
/// parsing operation.
/// </summary>
/// <param name="input">An input <c>string</c>.</param>
/// <returns>Returns an <c>Input</c>.</returns>
let prepare(input: string) : Input = input, 0, false

/// <summary>
/// Use this to prepare a rich string (an <c>Input</c>) for debug-mode
/// parsing operation.
/// </summary>
/// <param name="input">An input <c>string</c>.</param>
/// <returns>Returns an <c>Input</c>.</returns>
let debug(input: string) : Input = input, 0, true

/// <summary>
/// Extracts the string input from an <c>Input</c> tuple.
/// </summary>
/// <param name="input">An <c>Input</c>.</param>
/// <returns>The input <c>string</c>.</returns>
let input i =
    let (e,_,_) = i
    e

/// <summary>
/// Extracts the current position from an <c>Input</c> tuple.
/// </summary>
/// <param name="input">An <c>Input</c>.</param>
/// <returns>The position <c>int</c>.</returns>
let position i =
    let (_,e,_) = i
    e

/// <summary>
/// Returns <c>true</c> if the <c>Input</c>'s current position
/// is at the end of the input <c>string</c> ("end of file").
/// </summary>
/// <param name="input">An <c>Input</c>.</param>
/// <returns><c>true</c> iff the position is EOF.</returns>
let isEOF i =
    let pos = position i
    let len = String.length (input i)
    pos >= len

/// <summary>
/// Returns <c>true</c> if the <c>Input</c> is running in
/// debug mode.
/// </summary>
/// <param name="input">An <c>Input</c>.</param>
/// <returns><c>true</c> iff debug mode enabled.</returns>
let isDebug i =
    let (_,_,e) = i
    e

/// Represents the result of running a <c>Parser<'a></c>.
type Outcome<'a> =
| Success of result: 'a * remaining: Input
| Failure of fail_pos: int * rule: string

/// A <c>Parser<'a></c> is a function from <c>Input</c> to
/// <c>Outcome<'a></c>.
type Parser<'a> = Input -> Outcome<'a>

/// <summary>
/// <c>recparser</c> is used to declare a parser before it is
/// defined.  The primary use case is when defining recursive
/// parsers, e.g., parsers of the form <c>e ::= ... e ...</c>.
/// </summary>
/// <returns>A tuple containing a simple parser that calls an
/// implementation stored in a mutable reference cell, and a
/// mutable reference cell initialized to hold a dummy
/// implementation.</returns>
let recparser() =
  let dumbparser = fun (input: Input) -> failwith "You forgot to initialize your recursive parser."
  let r = ref dumbparser
  (fun (input: Input) -> !r input), r

// suggested refactoring in RFC FS-1111 due to ref cell deprecation
// https://github.com/fsharp/fslang-design/blob/main/FSharp-6.0/FS-1111-refcell-op-information-messages.md
// to be enabled CSCI 334, Spring 2024
// type 'a RefCell = { Value: 'a }

// let recparser() =
//     let dumbparser = fun (input: Input) -> failwith "You forgot to initialize your recursive parser."
//     let r = { Value = dumbparser }
//     (fun (input: Input) -> r.Value input), r

/// <summary>
/// Returns the hexadecimal character code for the given character.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns>A <c>string</c> representing a char code, in hex.</returns>
let cToHex(c: char) = "0x" + System.Convert.ToByte(c).ToString("x2");;

/// <summary>
/// A debug parser.  Prints debug information for the given parser
/// <c>p</c> as a side effect.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="label">A <c>string</c> label for <c>p</c> in the output.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>An <c>Outcome<'a>, or if <c>i</c> is omitted, a <c>Parser<'a></c>.</returns>
let (<!>)(p: Parser<'a>)(label: string)(i: Input) : Outcome<'a> =
    // if debugging is enabled...
    if (isDebug i) then
        let nextText = (input i).Substring(position i)
        if (input i).Length - (position i) > 0 then
            eprintfn "[attempting: %s on \"%s\", next char: %s]" label nextText (cToHex (input i).[0])
        else
            eprintfn "[attempting: %s on \"%s\", next char: EOF]" label nextText
        let o = p i
        match o with
        | Success(a, i') ->
            let i1pos = position i
            let i2pos = position i'
            let istr = input i
            let nconsumed = i2pos - i1pos
            let iconsumed = istr.Substring(i1pos, i2pos - i1pos)
            let rem = istr.[i2pos..]
            if istr.Length - i2pos > 0 then
                eprintfn "[success: %s, consumed: \"%s\", remaining: \"%s\", next char: %s]" label iconsumed rem (cToHex rem.[0])
            else
                eprintfn "[success: %s, consumed: \"%s\", remaining: \"%s\", next char: EOF]" label iconsumed rem
        | Failure(pos,rule) ->
            let rem = (input i).[pos..]
            if rem.Length > 0 then
                eprintfn "[failure at pos %d in rule [%s]: %s, remaining input: \"%s\", next char: %s]" pos rule label rem (cToHex rem.[0])
            else
                eprintfn "[failure at pos %d in rule [%s]: %s, remaining input: \"%s\", next char: EOF]" pos rule label rem
        o
    // if debugging is disabled
    else
        p i

/// <summary>
/// Returns <c>true</c> if the given regular expression <c>rgx</c> matches <c>s</c>.
/// </summary>
/// <param name="s">A <c>string</c>.</param>
/// <param name="rgx">A <c>string</c> representing a C# regular expression.</param>
/// <returns><c>true</c> iff <c>rgx</c> matches <c>s</c>.</returns>
let is_regexp(s: string)(rgx: string) =
    Regex.Match(s, rgx).Success

/// <summary>
/// Returns <c>true</c> if the given character is whitespace.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is whitespace.</returns>
let is_whitespace(c: char) = is_regexp (c.ToString()) @"\s"

/// <summary>
/// Returns <c>true</c> if the given character is whitespace,
/// not including newline characters.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is whitespace but not newline.</returns>
let is_whitespace_no_nl(c: char) = is_regexp (c.ToString()) @"\t| "

/// <summary>
/// Returns <c>true</c> if the given character is uppercase.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is uppercase.</returns>
let is_upper(c: char) = is_regexp (c.ToString()) @"[A-Z]"

/// <summary>
/// Returns <c>true</c> if the given character is lowercase.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is lowercase.</returns>
let is_lower(c: char) = is_regexp (c.ToString()) @"[a-z]"

/// <summary>
/// Returns <c>true</c> if the given character is a letter.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is a letter.</returns>
let is_letter(c: char) = is_upper c || is_lower c

/// <summary>
/// Returns <c>true</c> if the given character is a numeric digit.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns><c>true</c> iff <c>c</c> is a numeric digit.</returns>
let is_digit(c: char) = is_regexp (c.ToString()) @"[0-9]"

/// <summary>
/// Consumes nothing from the given <c>Input</c>, returning <c>a</a>.
/// </summary>
/// <param name="a">Any value.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<a></c> that is always <c>Success(a)</c>.</returns>
let presult(a: 'a)(i: Input) : Outcome<'a> = Success(a,i)

/// <summary>
/// Consumes nothing from the given <c>Input</c> and fails.
/// </summary>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a></c> that is always <c>Failure</c>.</returns>
let pzero(i: Input) : Outcome<'a> = Failure((position i), "pzero")

/// <summary>
/// Consumes a single character from the given <c>Input</c>.
/// </summary>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns a <c>Parser</c> that succeeds with a single <c>char</c>.</returns>
let pitem(i: Input) : Outcome<char> =
    let pos = position i
    let istr = input i
    if pos >= String.length istr then
        Failure ((position i),"pitem")
    else
        let debug = isDebug i
        let pos = position i
        Success (istr.[pos], (istr, pos + 1, debug))

/// <summary>
/// Runs <c>p</c> and then calls <c>f</c> on the result, yielding
/// a new parser that is a function of the first parser's result.
/// If an <c>Input</c> is also given, also runs the second parser.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="f">A function that takes an <c>'a</c> and yields a new <c>Parser<'b></c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a></c>.</returns>
let pbind(p: Parser<'a>)(f: 'a -> Parser<'b>)(i: Input) : Outcome<'b> =
    match p i with
    | Success(a,i') -> f a i'
    | Failure(pos,rule) -> Failure(pos,rule)

/// <summary>
/// Runs <c>p1</c> and, if it succeeds, runs <c>p2</c> on the
/// remaining input.  If both <c>p1</c> and <c>p2</c> succeed,
/// runs <c>f</c> on the pair of results.
/// </summary>
/// <param name="p1">A <c>Parser<'a></c>.</param>
/// <param name="p2">A <c>Parser<'a></c>.</param>
/// <param name="f">A function that takes the results of <c>p1</c>
/// and <c>p2</c> and returns a user-defined function of the two.</param>
let pseq(p1: Parser<'a>)(p2: Parser<'b>)(f: 'a*'b -> 'c) : Parser<'c> =
    pbind p1 (fun a ->
        pbind p2 (fun b ->
            presult (f (a,b))
        )
    )

/// <summary>
/// Overrides the failure cause returned by a failing parser.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="rule">A <c>string</c> describing <c>p</c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a></c>.</returns>
let cause(p: Parser<'a>)(rule: String)(i: Input) : Outcome<'a> =
    let o = p i
    match o with
    | Success _ -> o
    | Failure(pos,_) -> Failure(pos, rule)

/// <summary>
/// Checks whether the current character matches a predicate.
/// Useful for checking whether a character matches a set of characters.
/// </summary>
/// <param name="f">A function from <c>char -> bool</c>.</param>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let psat(f: char -> bool) : Parser<char> =
    cause
        (pbind pitem (fun c -> if (f c) then presult c else pzero))
        "psat"

/// <summary>
/// Checks whether the current character matches a given character.
/// </summary>
/// <param name="c">A <c>char</c>.</param>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let pchar(c: char) : Parser<char> =
    cause
        (psat (fun c' -> c' = c))
        (sprintf "pchar '%c'" c)

/// <summary>
/// Checks whether the current character is a letter.
/// </summary>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let pletter : Parser<char> =
    cause
        (psat is_letter)
        "is_letter"

/// <summary>
/// Checks whether the current character is a numeric digit.
/// </summary>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let pdigit : Parser<char> =
    cause
        (psat is_digit)
        "is_digit"

/// <summary>
/// Checks whether the current character is an uppercase letter.
/// </summary>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let pupper : Parser<char> =
    cause
        (psat is_upper)
        "is_upper"

/// <summary>
/// Checks whether the current character is a lowercase letter.
/// </summary>
/// <returns>Returns a <c>Parser<char></c>.</returns>
let plower : Parser<char> =
    cause
        (psat is_lower)
        "is_lower"

/// <summary>
/// Allows parsing alternatives.  First tries <c>p1</c> and if that
/// fails, tries <c>p2</c>.  Returns <c>Success</c> if either
/// <c>p1</c> or <c>p2</c> succeeds, and failure otherwise. Note that
/// both parser alternatives must return the same type.
/// </summary>
/// <param name="p1">A <c>Parser<'a></c>.</param>
/// <param name="p2">A <c>Parser<'a></c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a></c> or a <c>Parser<'a></c> if <c>i</c> is omitted.</returns>
let (<|>)(p1: Parser<'a>)(p2: Parser<'a>)(i: Input) : Outcome<'a> =
    let o = p1 i
    match o with
    | Success(_,_)      -> o
    | Failure(pos,rule) ->
        let o2 = p2 i
        match o2 with
        | Success(_,_)  -> o2
        | Failure(pos2,rule2) ->
            // return the furthest failure
            if pos >= pos2 then
                Failure(pos,rule)
            else
                Failure(pos2,rule2)

/// <summary>
/// Runs <c>p</c>, and when it succeeds, runs a function <c>f</c>
/// to transform the output of <c>p</c>.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="f">A transformer function <c>'a -> 'b</c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'b></c> or a <c>Parser<'b></c> if <c>i</c> is omitted.</returns>
let pfun(p: Parser<'a>)(f: 'a -> 'b)(i: Input) : Outcome<'b> =
    let o = p i
    match o with
    | Success(a,i') -> Success(f a, i')
    | Failure(pos,rule) -> Failure(pos,rule)

/// <summary>
/// Runs <c>p</c>, and when it succeeds, runs a function <c>f</c>
/// to transform the output of <c>p</c>. This is syntactic sugar
/// for the <c>pfun</c> function so that <c>pfun</c> can be used
/// inline, ala <c>p |>> f</c>.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="f">A transformer function <c>'a -> 'b</c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'b></c> or a <c>Parser<'b></c> if <c>i</c> is omitted.</returns>
let (|>>)(p: Parser<'a>)(f: 'a -> 'b) : Parser<'b> = pfun p f

/// <summary>
/// The parser equivalent of a constant function.  Runs <c>p</c> and if it
/// succeeds, returns <c>x</c>.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="x">Any value.</param>
/// <returns>Returns a <c>Parser<'b></c>.</returns>
let pfresult(p: Parser<'a>)(x: 'b) : Parser<'b> =
    pbind p (fun _ -> presult x)

/// <summary>
/// Runs <c>p</c> zero or more times.  Always runs until <c>p</c> fails at
/// least once.  If <c>p</c> is incapable of failing, this will loop forever,
/// so don't do that.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a list></c> or a <c>Parser<'a list></c> if <c>i</c> is omitted.</returns>
let rec pmany0(p: Parser<'a>)(i: Input) : Outcome<'a list> =
    let rec pm0(xs: 'a list)(i: Input) : Outcome<'a list> =
        match p i with
        | Failure(pos,rule) -> Success(xs, i)
        | Success(a, i')    ->
            if i = i' then
                failwith "pmany parser loops infinitely!"
            pm0 (a::xs) i'
    match pm0 [] i with
    | Success(xs,i')    -> Success(List.rev xs, i')
    | Failure(pos,rule) -> Failure(pos,rule)

/// <summary>
/// Runs <c>p</c> one or more times.  Always runs until <c>p</c> fails at
/// least once.  If <c>p</c> is incapable of failing, this will loop forever,
/// so don't do that.
/// </summary>
/// <param name="p">A <c>Parser<'a></c>.</param>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<'a list></c> or a <c>Parser<'a list></c> if <c>i</c> is omitted.</returns>
let pmany1(p: Parser<'a>) : Parser<'a list> =
    pseq p (pmany0 p) (fun (x,xs) -> x :: xs)

/// <summary>
/// Consumes zero or more whitespace characters, excluding newlines.
/// </summary>
/// <returns>Returns a <c>Parser<char list></c>.</returns>
let pwsNoNL0 : Parser<char list> = pmany0 (psat is_whitespace_no_nl)

/// <summary>
/// Consumes one or more whitespace characters, excluding newlines.
/// </summary>
/// <returns>Returns a <c>Parser<char list></c>.</returns>
let pwsNoNL1 : Parser<char list> = pmany1 (psat is_whitespace_no_nl)

/// <summary>
/// Consumes zero or more whitespace characters.
/// </summary>
/// <returns>Returns a <c>Parser<char list></c>.</returns>
let pws0 : Parser<char list> =
    cause
        (pmany0 (psat is_whitespace))
        "pws0"

/// <summary>
/// Consumes one or more whitespace characters.
/// </summary>
/// <returns>Returns a <c>Parser<char list></c>.</returns>
let pws1 : Parser<char list> =
    cause
        (pmany1 (psat is_whitespace))
        "pws1"

/// <summary>
/// Consumes the given string.
/// </summary>
/// <param name="s">A <c>string</c>.</param>
/// <returns>Returns a <c>Parser<string></c>.</returns>
let pstr(s: string) : Parser<string> =
    cause
        (s.ToCharArray()
        |> Array.fold (fun pacc c ->
                          pseq pacc (pchar c) (fun (s,ch) -> s + ch.ToString())
                      ) (presult ""))
        (sprintf "pstr \"%s\"" s)

/// <summary>
/// Consumes only the newline character.  Should work for both UNIX and
/// Windows line endings.
/// </summary>
/// <returns>Returns a <c>Parser<string></c>.</returns>
let pnl : Parser<string> =
    cause
        ((psat (fun c -> c = '\n') |>> (fun c -> c.ToString()))
        <|> (pstr "\r\n"))
        "pnl"

/// <summary>
/// Consumes the end of file.  Run this to ensure that the entire
/// input has been parsed.
/// </summary>
/// <param name="i">An <c>Input</c>.</param>
/// <returns>Returns an <c>Outcome<bool></c> or a <c>Parser<bool></c> if <c>i</c> is omitted.</returns>
let peof(i: Input) : Outcome<bool> =
    match pitem i with
    | Failure(pos,rule) ->
        if isEOF i then
            Success(true, i)
        else
            Failure(pos, rule)
    | Success(_,_) -> Failure((position i), "peof")

/// <summary>
/// Runs <c>pleft</c> and <c>pright</c>, returning only the result of <c>pleft</c> if
/// both parsers succeed.
/// </summary>
/// <param name="pleft">A <c>Parser<'a></c>.</param>
/// <param name="right">A <c>Parser<'b></c>.</param>
/// <returns>Returns a <c>Parser<'a></c>.</returns>
let pleft(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'a> =
    pbind pleft (fun a -> pfresult pright a)

/// <summary>
/// Runs <c>pleft</c> and <c>pright</c>, returning only the result of <c>pright</c> if
/// both parsers succeed.
/// </summary>
/// <param name="pleft">A <c>Parser<'a></c>.</param>
/// <param name="right">A <c>Parser<'b></c>.</param>
/// <returns>Returns a <c>Parser<'b></c>.</returns>
let pright(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'b> =
    pbind pleft (fun _ -> pright)

/// <summary>
/// Runs <c>poen</c>, then <c>p</c>, the <c>pclose</c>, returning only the result of <c>p</c> if
/// all three parsers succeed.
/// </summary>
/// <param name="popen">A <c>Parser<'a></c>.</param>
/// <param name="p">A <c>Parser<'b></c>.</param>
/// <param name="pclose">A <c>Parser<'c></c>.</param>
/// <returns>Returns a <c>Parser<'b></c>.</returns>
let pbetween(popen: Parser<'a>)(p: Parser<'b>)(pclose: Parser<'c>) : Parser<'b> =
    pright popen (pleft p pclose)

/// <summary>
/// Turns a list of characters into a <c>string</c>.
/// </summary>
/// <param name="cs">A <c>char list</c>.</param>
/// <returns>Returns a <c>string</c>.</returns>
let stringify(cs: char list) : string = String.Join("", cs)

(* do not call directly *)
let rec leftpad str ch num =
    if num > 0 then
        leftpad (ch.ToString() + str) ch (num - 1)
    else
        str

(* do not call directly *)
let windowLeftIndex(window_sz: int)(failure_pos: int) : int =
    if failure_pos - window_sz < 0 then
        0
    else
        failure_pos - window_sz

(* do not call directly *)
let windowRightIndex(window_sz: int)(failure_pos: int)(buffer_len: int) : int =
    if failure_pos + window_sz >= buffer_len then
        buffer_len - 1
    else
        failure_pos + window_sz

(* do not call directly *)
let indexOfLastNewlineLeftWindow(left_index: int)(failure_pos: int)(buffer: string) : int =
    // search for last occurrence of '\n'
    let rec searchBackward(pos: int) : int option =
        if pos <= left_index then
            None
        else if buffer.[pos] = '\n' then
            Some pos
        else
            searchBackward (pos - 1)

    match searchBackward (failure_pos - 1) with
    | Some idx -> idx
    | None -> left_index

(* do not call directly *)
let indexOfFirstNewlineRightWindow(right_index: int)(failure_pos: int)(buffer: string) : int =
    // search for first occurrence of '\n'
    let rec searchForward(pos: int) : int option =
        if pos >= right_index then
            None
        else if buffer.[pos] = '\n' then
            Some pos
        else
            searchForward (pos + 1)

    match searchForward (failure_pos + 1) with
    | Some idx -> idx
    | None -> right_index

/// <summary>
/// Produce a diagnostic message for a parser failure.
/// </summary>
/// <param name="window_sz">The amount of context (in chars) to show to the left and right of the failure position.</param>
/// <param name="failure_pos">Where the parse failed.</param>
/// <param name="buffer">The input stream.</param>
/// <param name="err">The error message.</param>
/// <returns>Returns a diagnostic <c>string</c>.</returns>
let diagnosticMessage(window_sz: int)(failure_pos: int)(buffer: string)(err: string) : string =
    // compute window
    let left_idx = windowLeftIndex window_sz failure_pos
    let right_idx = windowRightIndex window_sz failure_pos buffer.Length
    let last_nl_left = indexOfLastNewlineLeftWindow left_idx failure_pos buffer
    let first_nl_right = indexOfFirstNewlineRightWindow right_idx failure_pos buffer

    // find caret position in last line
    let caret_pos = failure_pos - last_nl_left + 1

    // create window string
    let window = buffer.Substring(left_idx, failure_pos - left_idx + 1 + right_idx - failure_pos)

    // augment with diagnostic info
    let diag = err + "\n\n" + window + "\n" + (leftpad "^" ' ' (caret_pos - 1)) + "\n"

    diag
