(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open PlainSyntaxAccessors

module Make
(G : sig
  val grammar: grammar
end)
(X : sig
  val topological_numbering: bool
end)
() = MenhirMrk.GrammarConstruction.Make (struct
  type nonrec ocamltype = ocamltype
  type nonrec merge_fun = merge_fun
  type nonrec attributes = attributes
  type nonrec action = action

open G

(* -------------------------------------------------------------------------- *)

(* [index] indexes a list of (distinct) strings, that is, assigns an
   integer index to each string and builds mappings both ways between
   strings and indices. *)

(* The order of the list is respected: the [i]-th element of the list
   [strings] receives the index [i]. *)

let index (strings : string list) : int * string array * int StringMap.t =
  let name = Array.of_list strings
  and n, map = List.fold_left (fun (n, map) s ->
    n+1, StringMap.add s n map
  ) (0, StringMap.empty) strings in
  assert (StringMap.cardinal map = n);
  n, name, map

(* -------------------------------------------------------------------------- *)

(* Nonterminal symbols. *)

module Nonterminal = struct

  type t = int

  (* Determine how many nonterminals we have, and build mappings both ways
     between names and indices. For each user start symbol [S], an internal
     start symbol [S'] is created. No name collision is possible because the
     quote character ['] cannot occur in the name of a symbol. *)

  let new_start_nonterminals =
    StringSet.fold (fun symbol ss -> (symbol ^ "'") :: ss) grammar.start_symbols []

  let original_nonterminals =
    nonterminals grammar

  let start =
    List.length new_start_nonterminals

  let (n : int), (name : string array), (map : int StringMap.t) =
    index (new_start_nonterminals @ original_nonterminals)

  let is_internal_start nt =
    nt < start

  let is_user_start nt =
    StringSet.mem name.(nt) grammar.start_symbols

  let lookup symbol =
    StringMap.find symbol map

  let print normalize nt =
    if normalize then
      MString.normalize name.(nt)
    else
      name.(nt)

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let ocamltype : t -> ocamltype option =
    tabulate @@ fun nt ->
    try
      Some (StringMap.find name.(nt) grammar.types)
    with Not_found ->
      None

  let ocamltype nt =
    assert (not (is_internal_start nt));
    ocamltype nt

  let ocamltype_of_start_symbol nt =
    Option.get (ocamltype nt)

  let positions : t -> ranges =
    tabulate @@ fun nt ->
    if is_internal_start nt then []
    else (StringMap.find name.(nt) grammar.rules).positions

  let attributes : attributes array =
    Array.make n []

  let merge_function : merge_fun option array =
    Array.make n None

  let () =
    grammar.rules |> StringMap.iter @@ fun symbol rule ->
    let nt = lookup symbol in
    attributes.(nt) <- rule.attributes;
    merge_function.(nt) <- rule.merge

  let attributes nt =
    attributes.(nt)

  let merge_function nt =
    merge_function.(nt)

  (* Build a list of the user start symbols and sort it, so that
     elements appear in the correct order. *)

  let user_start_symbols =
    grammar.start_symbols
    |> StringSet.elements
    |> List.map lookup
    |> List.sort Int.compare

  (* Generating the [state] GADT (in the code back-end) or generating the
     inspection API (in the table back-end) requires that the type of every
     nonterminal symbol be known. *)

  let symbols_without_ocaml_type () =
    foldx (fun nt accu ->
      match ocamltype nt with
      | Some _ ->
          accu
      | None ->
          nt :: accu
    ) []

  let check_every_symbol_has_ocaml_type reason =
    let nts = symbols_without_ocaml_type() in
    if nts <> [] then
      let b = Buffer.create 1024 in
      Printf.bprintf b "\
        the %s requires the type of every nonterminal symbol to be\n\
        known. Please specify the type of every symbol via %%type declarations, or\n\
        enable type inference (look up --infer in the manual).\n\
        Type inference is automatically enabled when Menhir is used via Dune,\n\
        provided the dune-project file says (using menhir 2.0) or later.\n"
        reason
      ;
      Printf.bprintf b "The types of the following nonterminal symbols are unknown:";
      List.iter (fun nt ->
        Printf.bprintf b "\n%s" (print false nt)
      ) nts;
      Report.Just.error [] "%s" (Buffer.contents b)

end (* Nonterminal *)

module NonterminalMap =
  IntMap.Make(Nonterminal)()

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

module Terminal = struct

  type t = int

  (* Determine how many terminals we have and build mappings
     both ways between names and indices. A new terminal "#"
     is created. A new terminal "error" is created. The fact
     that the integer code assigned to the terminal "#"
     is the last one is exploited in the table-based back-end.
     (The right-most row of the action table is not created.)

     Pseudo-tokens (used in %prec declarations, but never
     declared using %token) are filtered out. *)

  (* In principle, the number of the [error] token is irrelevant.
     It is currently 0, but we do not rely on that. *)

  let (n : int), (name : string array), (map : int StringMap.t) =
    index ("error" :: tokens grammar @ [ "#" ])

  let lookup name =
    StringMap.find name map

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  let print t =
    name.(t)

  let sharp =
    lookup "#"

  let error =
    lookup "error"

  let special t =
    t = sharp || t = error

  let real t =
    t <> sharp && t <> error

  let eof =
    try
      Some (lookup "EOF")
    with Not_found ->
      None

  let default = (* applicable to [error] and [#] *)
    {
      tk_precedence    = UndefinedPrecedence;
      tk_associativity = UndefinedAssoc;
      tk_ocamltype     = None;
      tk_is_declared   = true;
      tk_position      = Range.dummy;
      tk_attributes    = [];
      tk_alias         = None;
    }

  let properties =
    Array.init n @@ fun t ->
    try
      StringMap.find name.(t) grammar.tokens
    with Not_found ->
      assert (special t);
      default

  let ocamltype t =
    properties.(t).tk_ocamltype

  let precedence t =
    properties.(t).tk_precedence
  let associativity t =
    properties.(t).tk_associativity

  let attributes t =
    properties.(t).tk_attributes

  let alias t =
    properties.(t).tk_alias

  let unquoted_alias t =
    alias t
    |> Option.map MString.unquote

  let () =
    assert (sharp = n - 1)

  let[@inline] iterx f =
    MInt.iteri sharp f

  let[@inline] foldx f accu =
    MInt.foldi sharp f accu

  let[@inline] mapx f =
    MInt.mapi sharp f

  let[@inline] initx f =
    MInt.initi sharp f

  let () =
    assert (error = 0)

  let[@inline] iter_real f =
    MInt.iterij 1 sharp f

  let[@inline] fold_real f accu =
    MInt.foldij 1 sharp f accu

  let[@inline] map_real f =
    MInt.mapij 1 sharp f

  let tokens_without_an_alias =
    MRef.with_state 0 @@ fun accu ->
    iter_real @@ fun t ->
    if alias t = None then
      incr accu

  let every_token_has_an_alias =
    tokens_without_an_alias = 0

end (* Terminal *)

(* -------------------------------------------------------------------------- *)

module Symbol = struct
  type ('nonterminal, 'terminal) symbol = ('nonterminal, 'terminal) MenhirMrk.MiddleAPI.symbol =
    | N of 'nonterminal
    | T of 'terminal

  type t = (Nonterminal.t, Terminal.t) symbol

  let lookup name =
    try
      T (Terminal.lookup name)
    with Not_found ->
      try
        N (Nonterminal.lookup name)
      with Not_found ->
        assert false

  let non_error sym =
    match sym with
    | T t ->
        t <> Terminal.error
    | N _ ->
        true

  let print normalize sym =
    match sym with
    | N nt ->
        Nonterminal.print normalize nt
    | T t ->
        Terminal.print t

  let print_subarray normalize start symbols =
    MString.subarray_bullet (print normalize) " " start (-1) "." symbols


end 

(* -------------------------------------------------------------------------- *)

(* Productions. *)

module Production = struct

  type t =
    int

  (* For every start symbol [S], a new production S' -> S is created.
     It is known as a start production. *)

  (* Count how many productions we have, including the start productions.
     This is [n]. *)

  let n : int =
    let n = StringMap.fold (fun _ rule n ->
      n + List.length rule.branches
    ) grammar.rules 0 in
    n + StringSet.cardinal grammar.start_symbols

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  (* Create a number of uninitialized tables that map a production index to
     information about this production. *)

  (* [table] maps a production to the left-hand side and right-hand side of
     this production. [identifiers] maps a production to an array of the
     identifiers that are used to name the elements of the right-hand side.
     [actions] maps a production to an optional semantic action. (Only the
     start productions have none.) [positions] maps a production to an array
     of the positions (in the .mly file) of the elements of the right-hand
     side. [rhs_attributes] maps a production to an array of the attributes
     attached to the elements of the right-hand side. [prec_decl] maps a
     production to an optional [%prec] annotation. [production_level] maps a
     production to a production level. [attributes] maps a production to the
     list of its attributes. *)

  let table : (Nonterminal.t * Symbol.t array) array =
    Array.make n (-1, [||])

  let identifiers : identifier array array =
    Array.make n [||]

  let actions : action option array =
    Array.make n None

  let positions : Range.range list array =
    Array.make n []

  let rhs_attributes : attributes array array =
    Array.make n [||]

  let prec_decl : (string located * precedence_level) option array =
    Array.make n None

  let production_level : production_level array =
    (* The start productions receive a level that pretends that they
       originate in a fictitious "builtin" file. So, a reduce/reduce
       conflict that involves a start production will not be solved. *)
    let dummy = ProductionLevel (InputFile.name InputFile.builtin, 0) in
    Array.make n dummy

  let attributes : attributes array =
    Array.make n []

  (* [ntprods] maps a nonterminal symbol to the interval of its productions. *)

  let ntprods : (int * int) array =
    Array.make Nonterminal.n (-1, -1)

  (* Create the start productions, populating the above arrays as appropriate.
     [start] is the number of start productions, therefore also the index of
     the first non-start production. [startprods] is a mapping of the start
     symbols to the corresponding start productions. *)

  let (start : int), (startprods : t NonterminalMap.t) =
    StringSet.fold (fun nonterminal (k, startprods) ->
      let nt = Nonterminal.lookup nonterminal
      and nt' = Nonterminal.lookup (nonterminal ^ "'") in
      table.(k) <- (nt', [| Symbol.N nt |]);
      identifiers.(k) <- [| "_1" |];
      ntprods.(nt') <- (k, k+1);
      positions.(k) <- Nonterminal.positions nt;
        (* A start production has no attributes. *)
      k+1,
      NonterminalMap.add nt k startprods
    ) grammar.start_symbols (0, NonterminalMap.empty)

  let[@inline] is_start prod =
    prod < start

  let start_production nt =
    try
      NonterminalMap.find nt startprods
    with Not_found ->
      assert false (* [nt] is not a start symbol *)

  (* We are now about to enumerate and number the non-start productions. There
     is a twist. If [X.topological_numbering] is [false] then we do so in an
     arbitrary order. If [X.topological_numbering] is [true] and if the
     grammar is acyclic then we number the productions in such a way that if
     [A → α] receives a smaller number than [B → β] then A => B is permitted
     but B => A is impossible. We exploit this property in the GLR parsing
     algorithm. *)

  let foreach_rule : (nonterminal -> rule -> 's -> 's) -> 's -> 's =
    (* An arbitrary ordering. *)
    fun yield s -> StringMap.fold yield grammar.rules s

  let foreach_rule : (nonterminal -> rule -> 's -> 's) -> 's -> 's =
    if X.topological_numbering then
      let module O = TopologicalOrdering.Make(G) in
      match O.nonterminals with
      | None ->
          (* The grammar is cyclic. Abandon silently. *)
          foreach_rule
      | Some nts ->
          (* A topological ordering. *)
          fun yield s ->
            List.fold_left (fun s nt ->
              let rule = StringMap.find nt grammar.rules in
              yield nt rule s
            ) s nts
    else
      foreach_rule

  (* Create the non-start productions, populating the above arrays. *)

  let prod_symbol producer =
    Symbol.lookup producer.prod_symbol

  let (_ : int) = foreach_rule (fun nonterminal rule k ->
    let nt = Nonterminal.lookup nonterminal in
    let k' = MList.accumulate k rule.branches @@ fun k branch ->
      let producers = Array.of_list branch.producers in
      let rhs = Array.map prod_symbol producers in
      table.(k) <- (nt, rhs);
      identifiers.(k) <- Array.map prod_id producers;
      actions.(k) <- Some branch.action;
      rhs_attributes.(k) <- Array.map prod_attributes producers;
      production_level.(k) <- branch.production_level;
      prec_decl.(k) <- Option.map
       (fun terminal ->
          terminal,
          let tok = Located.value terminal in
          match StringMap.find tok grammar.tokens with
          | properties ->
            properties.tk_precedence
          | exception Not_found ->
            assert (tok = "error");
            UndefinedPrecedence)
       branch.prec_annotation;
      positions.(k) <- [ branch.branch_position ];
      attributes.(k) <- branch.br_attributes;
      k+1
    in
    ntprods.(nt) <- (k, k');
    k'
  ) start

  (* Accessors. *)

  let[@inline] def prod =
    table.(prod)

  let[@inline] nt prod =
    let nt, _ = table.(prod) in
    nt

  let[@inline] rhs prod =
    let _, rhs = table.(prod) in
    rhs

  let[@inline] length prod =
    Array.length (rhs prod)

  let[@inline] identifiers prod =
    identifiers.(prod)

  let action prod =
    match actions.(prod) with
    | Some action ->
        action
    | None ->
        (* Start productions have no action. *)
        assert (is_start prod);
        assert false

  let[@inline] positions prod =
    positions.(prod)

  let[@inline] attributes prod =
    attributes.(prod)

  let[@inline] rhs_attributes prod =
    rhs_attributes.(prod)

  let[@inline] prec_decl k =
    prec_decl.(k)

  let[@inline] production_level k =
    production_level.(k)

  let get_start prod =
    assert (is_start prod);
    match (rhs prod).(0) with
    | Symbol.N nt ->
        nt
    | Symbol.T _ ->
        assert false

  let test_start prod =
    if is_start prod then Some (get_start prod) else None

  let error_free prod =
    MArray.for_all Symbol.non_error (rhs prod)

  (* Printing a production. *)

  let print prod =
    assert (not (is_start prod));
    let nt, rhs = table.(prod) in
    let nt = Nonterminal.print false nt in
    if Array.length rhs = 0 then
      (* Avoid producing a trailing space. *)
      Printf.sprintf "%s ->" nt
    else
      Printf.sprintf "%s -> %s" nt
        (Symbol.print_subarray false 0 rhs)

  let describe gerund prod =
    match test_start prod with
    | Some nt ->
        let ending = if gerund then "ing" else "" in
        Printf.sprintf "accept%s %s" ending (Nonterminal.print false nt)
    | None ->
        let ending = if gerund then "ing" else "e" in
        Printf.sprintf "reduc%s production %s" ending (print prod)

  (* Iteration. *)

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let[@inline] productions nt =
    let k, k' = ntprods.(nt) in
    MInt.interval k k'

  let[@inline] iternt nt f =
    let k, k' = ntprods.(nt) in
    MInt.iterij k k' f

  let[@inline] foldnt nt f accu =
    let k, k' = ntprods.(nt) in
    MInt.foldij k k' f accu

  let[@inline] mapnt nt f =
    let k, k' = ntprods.(nt) in
    MInt.mapij k k' f

end (* Production *)

(* -------------------------------------------------------------------------- *)
  let attributes = grammar.gr_attributes
  let on_error_reduce : on_error_reduce_level StringMap.t =
    grammar.on_error_reduce
end) ()
