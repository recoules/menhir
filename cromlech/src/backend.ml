(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MenhirLib
open MenhirMrk
open MiddleAPI

module type TOKEN = sig
  type t

  val encode : t -> int
  val value : t -> Obj.t
end

module type ACTION = sig
  type t
  type token

  val perform :
    t -> (int, Obj.t, token) EngineTypes.env -> (int, Obj.t) EngineTypes.stack
end

module type CONFIG = sig
  val debug : bool
  val compression : bool
end

module Make
    (Token : TOKEN)
    (Lr1 : sig
      include LR1_AUTOMATON

      val init : (t -> 'a) -> 'a array
      (**[init f] initializes a fresh array, indexed by the nodes. *)

      (* val start_node : Lr0.G.Nonterminal.t -> t *)
      (**[start_node nt] is the start node associated with the user nonterminal
         symbol [nt]. *)

      val has_default_reduction : t -> bool
      (**[has_default_reduction node] determines whether the node [node] has a
         default reduction. This function is a simplified variant of
         {!test_default_reduction}. *)

      val has_default_reduction_on_sharp : t -> bool
      (**[has_default_reduction_on_sharp node] determines whether the node
         [node] has a default reduction on the terminal symbol [#]. This
         function is a simplified variant of {!test_default_reduction}. *)
    end)
    (Action : ACTION with type t = Lr1.Lr0.G.action and type token = Token.t)
    (Config : CONFIG) : TableFormat.TABLES with type token = Token.t = struct
  module G = Lr1.Lr0.G

  type token = Token.t
  (**The type of tokens. *)

  (**[terminal_count] is the number of terminal symbols, without [#]. *)
  let terminal_count : int = G.Terminal.n - 1

  (**[token2terminal] maps a token to a terminal symbol, represented by its
     internal integer code. *)
  let token2terminal : token -> int = Token.encode

  (**[error_terminal] is the integer code of the special token [error]. *)
  let error_terminal : int = G.Terminal.encode G.Terminal.error

  (**[token2value] maps a token to its semantic value. *)
  let token2value : token -> Obj.t = Token.value

  (***************************************************************************)

  (* Table encodings. *)

  (* Encodings of entries in the default reduction table. *)

  let encode_DefRed prod =
    (* 1 + prod *)
    1 + G.Production.encode prod

  let encode_NoDefRed =
    (* 0 *)
    0

  (* Encodings of entries in the action table. *)

  let encode_Reduce prod =
    (* prod | 01 *)
    (G.Production.encode prod lsl 2) lor 1

  let encode_ShiftDiscard s =
    (*    s | 10 *)
    (Lr1.encode s lsl 2) lor 0b10

  let encode_ShiftNoDiscard s =
    (*    s | 11 *)
    (Lr1.encode s lsl 2) lor 0b11

  (* In the encoding of a shift transition, if the target state has a default
   reduction on [#], we use [ShiftNoDiscard], otherwise [ShiftDiscard]. *)
  let encode_Shift s =
    if Lr1.has_default_reduction_on_sharp s then encode_ShiftNoDiscard s
    else encode_ShiftDiscard s

  let encode_Fail =
    (*        00 *)
    0

  (* Encodings of entries in the goto table. *)

  let encode_Goto node =
    (* 1 + node *)
    1 + Lr1.encode node

  let encode_NoGoto =
    (* 0 *)
    0

  (* Encodings of entries in the error bitmap. *)

  let encode_Error =
    (* 0 *)
    0

  let encode_NoError =
    (* 1 *)
    1

  let emit_1D_array : int array -> int -> int =
    if Config.compression then fun array ->
      let bits, packed_array = PackedIntArray.pack array in
      PackedIntArray.get bits packed_array
    else Array.unsafe_get

  let emit_2D_sparse_matrix :
      int ->
      (int -> 'a) ->
      int ->
      (int -> 'b) ->
      ('a -> 'b -> int) ->
      (int -> bool) ->
      (int -> int -> int) * (int -> int -> int) =
    if Config.compression then
      fun n_row decode_row n_col decode_col value insignificant ->
      let matrix =
        Array.init n_row (fun i ->
            Array.init n_col (fun j -> value (decode_row i) (decode_col j)))
      in
      let displacement, data =
        Base.RowDisplacementEncode.compress insignificant 0 matrix
      in
      let get_displacement = emit_1D_array displacement
      and get_data = emit_1D_array data in
      ( (fun i j ->
          let k = MenhirLib.RowDisplacementDecode.decode (get_displacement i) in
          get_data (k + j)),
        fun i j -> matrix.(i).(j) )
    else fun n_row decode_row n_col decode_col value _ ->
      let matrix =
        Array.init (n_row * n_col) (fun k ->
            value (decode_row @@ (k / n_col)) (decode_col @@ (k mod n_col)))
      in
      let get i j = Array.unsafe_get matrix @@ ((n_col * i) + j) in
      (get, get)

  let emit_2D_bool_matrix :
      int ->
      (int -> 'a) ->
      int ->
      (int -> 'b) ->
      ('a -> 'b -> int) ->
      int ->
      int ->
      int =
    if Config.compression then fun n_row decode_row n_col decode_col value ->
      let size = n_row * n_col in
      let matrix =
        String.init
          ((size + 7) / 8)
          (fun i ->
            let byte = ref 0 in
            for j = 0 to 7 do
              let k = (8 * i) + j in
              byte :=
                (!byte lsl 1)
                lor
                if k < size then
                  value (decode_row @@ (k / n_col)) (decode_col @@ (k mod n_col))
                  land 1
                else 0
            done;
            Char.unsafe_chr !byte)
      in
      fun i j -> PackedIntArray.get1 matrix ((n_col * i) + j)
    else fun _ _ _ _ value -> value

  (***************************************************************************)

  (**Traditionally, an LR automaton is described by two tables, namely, an
     action table and a goto table. See, for instance, the Dragon book.

     The action table is a two-dimensional matrix that maps a state and a
     lookahead token to an action. An action is one of: shift to a certain
     state, reduce a certain production, accept, or fail.

     The goto table is a two-dimensional matrix that maps a state and a
     non-terminal symbol to either a state or undefined. By construction, this
     table is sparse: its undefined entries are never looked up. A compression
     technique is free to overlap them with other entries.

     In Menhir, things are slightly different. If a state has a default
     reduction on token [#], then that reduction must be performed without
     consulting the lookahead token. As a result, we must first determine
     whether that is the case, before we can obtain a lookahead token and use it
     as an index in the action table.

     Thus, Menhir's tables are as follows. *)

  (**The default reduction table, a one-dimensional table, maps a state to
     either ``no default reduction'' (encoded as: 0) or ``by default, reduce
     prod'' (encoded as: 1 + prod). The action table is looked up only when
     there is no default reduction. *)
  let default_reduction : int -> int =
    emit_1D_array @@ Lr1.init
    @@ fun node ->
    match Lr1.test_default_reduction node with
    | Some (prod, _) -> encode_DefRed prod
    | None -> encode_NoDefRed

  (**The action table, a two-dimensional table, maps a state and a terminal to
     one of ``shift to state s and discard the current token'' (encoded as:
     [s | 10]), ``shift to state s without discarding the current token''
     (encoded as: [es | 11]), or ``reduce prod'' (encoded as: [prod | 01]).

     Like the error bitmap, the action table does not contain a column for the
     [#] pseudo-terminal. *)
  let (action, internal_action) : (int -> int -> int) * (int -> int -> int) =
    emit_2D_sparse_matrix Lr1.n Lr1.decode G.Terminal.n G.Terminal.decode
      (fun node t ->
        (* If this state has a default reduction, then the action table is
     never looked up; this table entry is irrelevant. *)
        if Lr1.has_default_reduction node then encode_Fail
        else
          match G.SymbolMap.find (G.Symbol.T t) (Lr1.transitions node) with
          | target ->
              (* [node] has a transition to [target]. *)
              encode_Shift target
          | exception Not_found -> (
              match G.TerminalMap.find t (Lr1.reductions node) with
              | prods ->
                  (* [node] has a reduction. *)
                  let prod = Base.MList.single prods in
                  encode_Reduce prod
              | exception Not_found ->
                  (* [node] has no action. *)
                  encode_Fail))
      (( = ) encode_Fail)

  (** The error bitmap does not contain a column for the [#] pseudo-terminal.
      Thus, its width is [terminal_count]. *)
  let error : int -> int -> int =
    emit_2D_bool_matrix Lr1.n Fun.id G.Terminal.n Fun.id (fun i j ->
        if internal_action i j = encode_Fail then encode_Error
        else encode_NoError)

  (**A one-dimensional table, [lhs], maps a production to its left-hand side (a
     non-terminal symbol). *)
  let lhs : int -> int =
    emit_1D_array @@ G.Production.init
    @@ fun prod -> G.Nonterminal.encode (G.Production.nt prod)

  (**The goto table, a two-dimensional table, maps a state and a non-terminal
     symbol to either undefined (encoded as: 0) or a new state s (encoded as: 1
     \+ s).. *)
  let goto : int -> int -> int =
    fst
    @@ emit_2D_sparse_matrix Lr1.n Lr1.decode G.Nonterminal.n
         G.Nonterminal.decode
         (fun node nt ->
           try
             let target =
               G.SymbolMap.find (G.Symbol.N nt) (Lr1.transitions node)
             in
             encode_Goto target
           with Not_found -> encode_NoGoto)
         (( = ) encode_NoGoto)

  (**[start] is the number of start productions. A production [prod] is a start
     production if and only if [prod < start] holds. This is also the number of
     start symbols. A nonterminal symbol [nt] is a start symbol if and only if
     [nt < start] holds. *)
  let start : int = G.Production.start

  (**The semantic action table, a one-dimensional table, maps productions to
     semantic actions. The calling convention for semantic actions is described
     in [EngineTypes]. This table contains ONLY NON-START PRODUCTIONS, so the
     indexing is off by [start]. Be careful. *)
  let semantic_action :
      ((int, Obj.t, token) EngineTypes.env -> (int, Obj.t) EngineTypes.stack)
      array =
    Array.init (G.Production.n - G.Production.start) (fun i ->
        Action.perform
          (G.Production.action (G.Production.decode (G.Production.start + i))))

  exception Error = Parsing.Parse_error
  (**The exception [Error] can be raised by semantic actions, caught by the
     engine, and raised again by the engine for the final user to observe. *)

  (**[trace] indicates whether a trace should be generated. Generating a trace
     requires two extra tables, which respectively map a terminal symbol and a
     production to a string. *)
  let trace =
    if Config.debug then
      Some
        (G.Terminal.init G.Terminal.print, G.Production.init G.Production.print)
    else None
end
