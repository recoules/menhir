{
  open Cromlech
  open Cromlech.Grammar
  open Tokens 
}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']

rule token = parse
| '.'             { Token (t_dot, ()) }
| '+'             { Token (t_plus, ( + )) }
| '-'             { Token (t_minus, ( - )) }
| '*'             { Token (t_mul, ( * )) }
| '/'             { Token (t_div, ( / )) }
| '('             { Token (t_lparen, ()) }
| ')'             { Token (t_rparen, ()) }
| digit+ as s     { Token (t_int, int_of_string s) }
| space+          { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| eof             { Token (eof, ()) }
| _               { Token (error, ()) }
