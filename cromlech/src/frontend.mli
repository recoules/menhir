open MenhirMrk.MiddleAPI
open Types

val error : 'a terminal
val eof : 'a terminal

val terminal :
  ?position:range ->
  string ->
  associativity ->
  precedence_level ->
  'typ terminal

val nonterminal : ?inline:bool -> string -> 'typ nonterminal

val rule :
  ?source:range ->
  ?level:production_level ->
  ?precedence:string located * precedence_level ->
  'typ nonterminal ->
  ([< `Before | `Start | `Element | `Empty ], 'typ, 'action) producers ->
  'action ->
  rule
