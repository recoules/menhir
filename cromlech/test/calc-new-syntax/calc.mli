
module type SYMBOLS = sig
  
  val tk_times: (unit) Cromlech.Grammar.terminal
  
  val tk_rparen: (unit) Cromlech.Grammar.terminal
  
  val tk_plus: (unit) Cromlech.Grammar.terminal
  
  val tk_minus: (unit) Cromlech.Grammar.terminal
  
  val tk_lparen: (unit) Cromlech.Grammar.terminal
  
  val tk_int: (int) Cromlech.Grammar.terminal
  
  val tk_eol: (unit) Cromlech.Grammar.terminal
  
  val tk_div: (unit) Cromlech.Grammar.terminal
  
  val nt_main: (int) Cromlech.Grammar.nonterminal
  
end

module Symbols : SYMBOLS

module Grammar
(Symbols : SYMBOLS)
: sig
  
  val extend: (Cromlech.Grammar.rule) list -> (Cromlech.Grammar.rule) list
  
end
