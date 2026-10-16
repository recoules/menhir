{
  open Cromlech
  open Cromlech.Grammar
  open Calc.Symbols
}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']

rule token = parse
| '+'             { Token (tk_plus, ()) }
| '-'             { Token (tk_minus, ()) }
| '*'             { Token (tk_times, ()) }
| '/'             { Token (tk_div, ()) }
| '('             { Token (tk_lparen, ()) }
| ')'             { Token (tk_rparen, ()) }
| digit+ as s     { Token (tk_int, int_of_string s) }
| space+          { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| eof             { Token (tk_eol, ()) }
| _               { Token (error, ()) }
