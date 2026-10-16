open Cromlech
open Calc.Symbols
open Calc.Grammar (Calc.Symbols)
module Interpreter = (val interpreter [ nt_main ] (extend []))

let expr =
  match Interpreter.starts with
  | [ expr ] ->
      fun lexer lexbuf ->
        Interpreter.loop
          (Interpreter.lexer_lexbuf_to_supplier lexer lexbuf)
          (expr lexbuf.lex_curr_p)

let () =
  assert (expr Lexer.token (Lexing.from_string "((42) + 3 - 5 * 5 + 10)") = 30);
  assert (expr Lexer.token (Lexing.from_string "60") = 60)
