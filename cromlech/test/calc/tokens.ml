open Cromlech.Grammar

let level0 : MenhirMrk.MiddleAPI.precedence_level =
  PrecedenceLevel
    (MenhirMrk.MiddleAPI.locate
       (Lexing.dummy_pos, Lexing.dummy_pos)
       ("<builtin>", 0))

let level1 : MenhirMrk.MiddleAPI.precedence_level =
  PrecedenceLevel
    (MenhirMrk.MiddleAPI.locate
       (Lexing.dummy_pos, Lexing.dummy_pos)
       ("<builtin>", 1))

let t_int : int terminal = terminal "INT" UndefinedAssoc UndefinedPrecedence
let t_plus : (int -> int -> int) terminal = terminal "PLUS" LeftAssoc level0
let t_minus : (int -> int -> int) terminal = terminal "MINUS" LeftAssoc level0
let t_mul : (int -> int -> int) terminal = terminal "MUL" LeftAssoc level1
let t_div : (int -> int -> int) terminal = terminal "DIV" LeftAssoc level1

let t_lparen : unit terminal =
  terminal "LPAREN" UndefinedAssoc UndefinedPrecedence

let t_rparen : unit terminal =
  terminal "RPAREN" UndefinedAssoc UndefinedPrecedence

let t_dot : unit terminal = terminal "DOT" UndefinedAssoc UndefinedPrecedence
