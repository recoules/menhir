open MenhirMrk.MiddleAPI

type 'typ terminal = {
  id : int;
  symbol : string;
  associativity : associativity;
  precedence : precedence_level;
  origin : range option;
}

type 'typ nonterminal = { id : int; symbol : string; inline : bool }

and 'typ parameter =
  | Terminal : 'typ terminal -> 'typ parameter
  | NonTerminal : 'typ nonterminal -> 'typ parameter

and any_parameter = Any : 'typ parameter -> any_parameter [@@unboxed]

and ('keyword, 'typ, 'action) producers =
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

type rule =
  | Rule :
      'typ nonterminal
      * ([< `Before | `Start | `Element | `Empty ], 'typ, 'action) producers
      * 'action
      * production_level option
      * (string located * precedence_level) option
      * range option
      -> rule
