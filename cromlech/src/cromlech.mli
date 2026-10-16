open MenhirMrk.MiddleAPI

module Grammar : sig
  type 'typ terminal

  type 'typ nonterminal

  and 'typ parameter =
    | Terminal : 'typ terminal -> 'typ parameter
    | NonTerminal : 'typ nonterminal -> 'typ parameter

  type ('keyword, 'typ, 'action) producers =
    | Ignore :
        'arg parameter
        * ([< `Start | `Element | `End | `Empty ], 'typ, 'action) producers
        -> ([< `Element ], 'typ, 'action) producers
    | Bind :
        'arg parameter
        * ([< `Start | `Element | `End | `Empty ], 'typ, 'action) producers
        -> ([< `Element ], 'typ, 'arg -> 'action) producers
    | Before :
        ([< `Start | `Element | `End | `Empty ], 'typ, 'action) producers
        -> ([< `Before ], 'typ, Lexing.position -> 'action) producers
    | Start :
        ([< `Element ], 'typ, 'action) producers
        -> ([< `Start ], 'typ, Lexing.position -> 'action) producers
    | End :
        ([< `Start | `Element | `Empty ], 'typ, 'action) producers
        -> ([< `End ], 'typ, Lexing.position -> 'action) producers
    | Return : ([< `Empty ], 'typ, 'typ) producers
    | ( :: ) :
        'arg parameter
        * ([< `Start | `Element | `End | `Empty ], 'typ, 'action) producers
        -> ([< `Element ], 'typ, 'arg -> 'action) producers
    | [] : ([< `Empty ], 'typ, 'typ) producers

  type rule

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
end

type token = Token : 'a Grammar.terminal * 'a -> token

module type INTERPRETER = sig
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

  type 'a starts =
    | ( :: ) :
        (Lexing.position -> 'a checkpoint) * 'b starts
        -> ('a -> 'b) starts
    | [] : unit starts

  type a

  val starts : a starts
end

type 'a interpreter = (module INTERPRETER with type a = 'a)

type 'a entries =
  | ( :: ) : 'a Grammar.nonterminal * 'b entries -> ('a -> 'b) entries
  | [] : unit entries

val interpreter :
  ?debug:bool ->
  ?compress:bool ->
  'a entries ->
  Grammar.rule list ->
  'a interpreter
