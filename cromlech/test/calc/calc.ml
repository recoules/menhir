open Cromlech.Grammar
open Tokens

let nt_op : (int -> int -> int) nonterminal = nonterminal ~inline:true "op"
let nt_expr : int nonterminal = nonterminal "expr"
let nt_expr_eof : int nonterminal = nonterminal "expr_eof"

let grammar : rule list =
  [
    rule nt_expr_eof
      (Bind (NonTerminal nt_expr, Ignore (Terminal t_dot, Return)))
      Fun.id;
    rule nt_expr [ Terminal t_int ] Fun.id;
    rule nt_expr
      (Ignore
         ( Terminal t_lparen,
           Bind (NonTerminal nt_expr, Ignore (Terminal t_rparen, Return)) ))
      Fun.id;
    rule nt_expr [ NonTerminal nt_expr; NonTerminal nt_op; NonTerminal nt_expr ]
      (fun x f y -> f x y);
    rule nt_op [ Terminal t_plus ] Fun.id;
    rule nt_op [ Terminal t_minus ] Fun.id;
    rule nt_op [ Terminal t_mul ] Fun.id;
    rule nt_op [ Terminal t_div ] Fun.id;
  ]

open Cromlech
module Interpreter = (val interpreter [ nt_expr_eof ] grammar)

let expr =
  match Interpreter.starts with
  | [ expr ] ->
      fun lexer lexbuf ->
        Interpreter.loop
          (Interpreter.lexer_lexbuf_to_supplier lexer lexbuf)
          (expr lexbuf.lex_curr_p)

let () =
  assert (expr Lexer.token (Lexing.from_string "((42) + 3 - 5 * 5 + 10).") = 30);
  assert (expr Lexer.token (Lexing.from_string "60.") = 60)
