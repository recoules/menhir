open MenhirMrk.MiddleAPI
open Types

let terminal :
    ?position:range ->
    string ->
    associativity ->
    precedence_level ->
    'typ terminal =
  let id = ref 0 in
  fun ?position symbol associativity precedence ->
    {
      id = Base.MInt.postincrement id;
      symbol;
      associativity;
      precedence;
      origin = position;
    }

let error : 'a terminal = terminal "error" UndefinedAssoc UndefinedPrecedence
let eof : 'a terminal = terminal "#" UndefinedAssoc UndefinedPrecedence

let nonterminal : ?inline:bool -> string -> 'typ nonterminal =
  let id = ref 0 in
  fun ?(inline = false) symbol ->
    { id = Base.MInt.postincrement id; symbol; inline }

let rule :
    ?source:range ->
    ?level:production_level ->
    ?precedence:string located * precedence_level ->
    'typ nonterminal ->
    ([< `Before | `Start | `Element | `Empty ], 'typ, 'action) producers ->
    'action ->
    rule =
 fun ?source ?level ?precedence nonterminal producers action ->
  Rule (nonterminal, producers, action, level, precedence, source)
