(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines several signatures that play an important role
   in the middle-end and back-end. *)

open FrontTypes

(* -------------------------------------------------------------------------- *)

(** {2 Numbered Types} *)

(**The signature [NUMBERING] describes a type [t] whose elements are in a
   bijection with an integer segment [\[0, n)]. This signature provides
   the integer constant [n] as well as the operations [encode], [decode].
   Please note that [decode] is considered {b unsafe} and should be used
   as little as possible. *)
module type NUMBERING =
  Fix.NUMBERING

(**The signature [NUMBERED] extends [NUMBERING] with more operations:
   [equal], [compare], [init], [tabulate], [iter], [fold], [map]. *)
module type NUMBERED =
  Fix.NUMBERED

(* -------------------------------------------------------------------------- *)

(**The signature [GRAMMAR] offers a rich view of a grammar. This view is
   well-suited to the computations that the middle-end performs. It offers
   abstract types of terminal symbols, nonterminal symbols, symbols, and
   productions, as well as efficient sets and maps. Furthermore, it offers
   access to the results of several analyses of the grammar, including
   NONEMPTY, NULLABLE, FIRST, FOLLOW, MINIMAL, MAXIMAL. The functor
   [GrammarConstruction.Make] implements this signature. *)
module type GRAMMAR = MenhirMrk.MiddleAPI.GRAMMAR
   with type ocamltype = ocamltype
    and type merge_fun = merge_fun
    and type attributes = attributes
    and type action = action

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[LR0] is the result signature of the functor [LR0Construction.Make].
   It offers access to the LR(0) automaton. Furthermore, it provides
   facilities for the efficient construction of LR(1) automata. *)
module type LR0 = MenhirMrk.MiddleAPI.LR0
    with type G.ocamltype = ocamltype
    and type G.merge_fun = merge_fun
    and type G.attributes = attributes
    and type G.action = action

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[MINIMAL_LR1_AUTOMATON] is the result signature of several LR(1) automaton
   construction algorithms. It is a minimal description of an LR(1) automaton.
   In this format, there is no description of the reductions: the reduction
   table at each node is implicit. It can be obtained by applying the function
   [Lr0.ALR1.reductions] to the state [state node]. *)
module type MINIMAL_LR1_AUTOMATON = MenhirMrk.MiddleAPI.MINIMAL_LR1_AUTOMATON
   with type Lr0.G.ocamltype = ocamltype
    and type Lr0.G.merge_fun = merge_fun
    and type Lr0.G.attributes = attributes
    and type Lr0.G.action = action

(**[LR1_AUTOMATON] is a richer description of an LR(1) automaton. In this
   format, there is an explicit description of the reductions. This format is
   sufficiently general to serve as the input and output format of several
   transformations, including silent conflict resolution, default conflict
   resolution, insertion of extra reductions on error, and introduction of
   default reductions. *)
module type LR1_AUTOMATON = MenhirMrk.MiddleAPI.LR1_AUTOMATON
   with type Lr0.G.ocamltype = ocamltype
    and type Lr0.G.merge_fun = merge_fun
    and type Lr0.G.attributes = attributes
    and type Lr0.G.action = action

(**Compared with [LR1_AUTOMATON], [LR1] is a more convenient description of
   an LR(1) automaton. It does not offer more information, but offers more
   facilities for accessing this information. *)
module type LR1 = MenhirMrk.MiddleAPI.LR1
   with type Lr0.G.ocamltype = ocamltype
    and type Lr0.G.merge_fun = merge_fun
    and type Lr0.G.attributes = attributes
    and type Lr0.G.action = action

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[SHAPE] is the result of an analysis of the shape of the stack in an LR(1)
   automaton.

   A stack shape describes just a suffix of the stack, not the whole stack.

   This shape information must be sound in the following sense: whenever there
   is an edge from [node1] to [node2] in the automaton, the shape at [node1],
   onto which a new cell is pushed, must flow into the shape at [node2]; where
   the relation "[shape1] flows into [shape2]" allows forgetting information
   by truncating a shape at its left end. For example, in the case of an
   analysis of the height of the stack, this soundness condition becomes: the
   height at [node1], plus 1, must be greater than or equal to the height at
   [node2]. In other words, the height at a node [node] must be at most the
   minimum of the heights at the predecessors of [node], plus one. *)
module type SHAPE = sig

  module G : GRAMMAR
    open G

  type node

  (**A shape is a description of the known suffix of the stack at a node. It
     is typically (but not necessarily) represented as an array of cells. By
     convention, the top of the stack is the right end of the array. *)
  type shape

  (**[node_shape node] is the shape of the stack at the node [node]. *)
  val node_shape: node -> shape

  (**[production_shape prod] is the shape of the stack at a node where the
     production [prod] can be reduced.

     In the short invariant, the length of this shape is [Production.length
     prod]. In the long invariant, its length can be greater.

     If there are no nodes where [prod] can be reduced, then this shape may be
     a bottom shape. By this I mean, in the case of the analysis [StackStates],
     that each stack cell may contain an empty set of nodes. *)
  val production_shape: Production.t -> shape

  (**[goto_shape nt] is the shape of the stack at a node where an edge
     labeled with the nonterminal symbol [nt] has just been followed.

     In the short invariant, the length of this shape is [1]: indeed, this
     shape consists of just one cell, associated with the symbol [nt].
     In the long invariant, its length can be greater. *)
  val goto_shape: Nonterminal.t -> shape

  (**[variant] distinguishes the short invariant and the long invariant. *)
  val variant: [`Short | `Long]

  (**[show_shape] produces a string representation of a shape. *)
  val show_shape: shape -> string

end

(**A kindergarten offers a set of operations on shapes. *)
module type KINDERGARTEN = sig

  (**A shape is a description of a suffix of the stack. It can be thought of
     as an array of cells. By convention, the top of the stack is the right
     end of the array. Thus, the index 0 in this array corresponds to the cell
     that lies deepest in the stack. *)
  type shape

  (**A cell is a description of a stack cell. *)
  type cell

  (**A symbol. *)
  type symbol

  (**A node. *)
  type node

  (**A set of nodes. *)
  type nodes

  (* Operations on shapes. *)

  (**[empty] is the empty shape. *)
  val empty: shape

  (**[get sh i] is the [i]-th cell in the shape [sh]. *)
  val get: shape -> int -> cell

  (**[length sh] is the length of the shape [sh]. *)
  val length: shape -> int

  (**[fold_left] enumerates the cells in a shape, from left to right. *)
  val fold_left: ('a -> cell -> 'a) -> 'a -> shape -> 'a

  (**[append sh1 sh2] is the concatenation of the shapes [sh1] and [sh2]. *)
  val append: shape -> shape -> shape

  (**[filter p sh] is the subsequence of the cells in the shape [sh]
     that satisfy the predicate [p]. *)
  val filter: (cell -> bool) -> shape -> shape

  (**[push sh cell] is the shape [sh], onto which the cell [cell]
     has been pushed. *)
  val push: shape -> cell -> shape

  (**[pop sh] is the shape [sh], deprived of its top element.
     The shape [sh] must be nonempty. *)
  val pop: shape -> shape

  (**[top sh] is the top element of the shape [sh].
     The shape [sh] must be nonempty. *)
  val top: shape -> cell

  (**[split sh k] splits the shape [sh] into two parts: a lower part of length
     [length sh - k] and an upper part of length [k]. The length of [sh] must
     be at least [k]. *)
  val split: shape -> int -> shape * shape

  (**[meet sh1 sh2] is the meet of the two shapes [sh1] and [sh2], that is, the
     logical conjunction of the information contained in [sh1] and [sh2]. It is
     computed roughly as follows. If the shapes [sh1] and [sh2] agree on a
     suffix of length [min (length sh1) (length s2)], then they are compatible.
     Then, their meet has length [max (length sh1) (length sh2)]. Otherwise,
     [sh1] and [sh2] are incompatible: they contradict each other. Then, their
     meet is bottom: [meet sh1 sh2] is [None]. *)
  val meet: shape -> shape -> shape option

  (**[show_shape sh] prints the sequence of symbols associated with the shape
     [sh]. *)
  val show_shape: shape -> string

  (**[to_list sh] converts the shape [sh] to a list of cells. *)
  val to_list: shape -> cell list

  (* Operations on cells. *)

  (**[symbol cell] is the symbol associated with the cell [cell]. This symbol
     determines the presence and the type of the semantic value stored in this
     cell. It also determines whether a start position and an end position are
     stored in this cell: see {!track_startp} and {!track_endp}. *)
  val symbol: cell -> symbol

  (**[states cell] is the set of the states that might be stored at runtime
     in this cell. The states in this set have the property that either all
     of them are represented, in which case [holds_state] is [true], or none
     of them is represented, in which case [holds_state] is [false]. *)
  val states: cell -> nodes

  (**[holds_semv cell] tells whether a semantic value is stored in the cell
     [cell]. By convention, if [symbol cell] is a nonterminal symbol, then a
     semantic value is stored. (We do not attempt to detect the situation where
     the semantic value could be omitted because it has type [unit], or the
     situation where it could be omitted because it is never used.) If [symbol
     cell] is a terminal symbol, then a semantic value is stored if and only if
     the [%token] declaration was annotated with a type. *)
  val holds_semv: cell -> bool

  (**[holds_state cell] tells whether a state is stored in the cell [cell]. *)
  val holds_state: cell -> bool

  (**[holds_startp cell] tells whether a start position is stored in the cell
     [cell]. It is equivalent to [track_startp (symbol cell)]. *)
  val holds_startp: cell -> bool

  (**[holds_endp cell] tells whether a start position is stored in the cell
     [cell]. It is equivalent to [track_endp (symbol cell)]. *)
  val holds_endp: cell -> bool

  (**[present cell] determines whether at least one of the four fields
     [state], [semv], [startp] or [endp] is present in the cell [cell]. *)
  val present: cell -> bool

  (**[similar] determines whether two stack cells have the same layout in
     memory, that is, the same OCaml type. This is equivalent to comparing
     all fields except [states]. *)
  val similar: cell -> cell -> bool

  (* Miscellaneous operations. *)

  (**[represented node] tells whether the node [node] has an explicit
     representation, that is, whether it is pushed onto the stack. *)
  val represented: node -> bool

  (**[track_startp symbol] tells whether a start position must be recorded
     for the symbol [symbol]. *)
  val track_startp: symbol -> bool

  (**[track_endp symbol] tells whether an end position must be recorded
     for the symbol [symbol]. *)
  val track_endp: symbol -> bool

end (* KINDERGARTEN *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Fragments of settings. *)

module type CONSTRUCTION_MODE_SETTINGS = MenhirMrk.MiddleAPI.CONSTRUCTION_MODE_SETTINGS 

module type NOPREFIX_SETTINGS = sig

  (**If [noprefix] is [false] then most identifiers begin with an unlikely
    prefix, in order to avoid name collisions. If [noprefix] is [true] then
    this prefix is removed. This makes the code more readable but can cause
    name collisions. This flag is not documented. *)
  val noprefix: bool

end

module type REPRESENT_SETTINGS = sig

  (**If [represent_states] is [true] then every stack cell is forced to
     contain a state. If it is [false] then an analysis is performed so as
     determine which stack cells do and do not need to hold a state. *)
  val represent_states: bool

  (**If [represent_positions] is [true] then every stack cell is forced to
     contain a start position and an end position. If it is [false] then an
     analysis is performed so as determine which stack cells do and do not
     need to hold a start position and an end position. *)
  val represent_positions: bool

  (**If [represent_values] is [true] then every stack cell is forced to
     contain a semantic value. If it is [false] then a stack cell that
     corresponds to a terminal symbol whose OCaml type is [unit] does not
     contain a semantic value. *)
  val represent_values: bool

end

module type STRATEGY_SETTINGS = sig

  (**[stragegy] determines which error-handling strategy the interpreter
     should use. For an explanation of the available strategies, see
     [IncrementalEngine]. *)
  val strategy: [`Legacy | `Simplified]

end

module type TRACE_SETTINGS = sig

  (**[trace] determines whether the interpreter should log its actions
     to the standard error channel. *)
  val trace: bool

end
