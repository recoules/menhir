open Base.Range
open MenhirMrk.MiddleAPI
open Types
open Frontend

let builtin = "<builtin>"

type 'a entries =
  | ( :: ) : 'a nonterminal * 'b entries -> ('a -> 'b) entries
  | [] : unit entries

type 'a parsers =
  | ( :: ) :
      ((Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a) * 'b parsers
      -> ('a -> 'b) parsers
  | [] : unit parsers

and token = Token : 'a terminal * 'a -> token

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

let sharp : unit terminal = eof

module rec Terminal : sig
  type t = T : 'a terminal -> t [@@unboxed]

  include Hashtbl.HashedType with type t := t
  module Hashtbl : Hashtbl.S with type key = t
end = struct
  type t = T : 'a terminal -> t [@@unboxed]

  let hash : t -> int = fun (T { id; _ }) -> id
  let equal : t -> t -> bool = ( == )

  module Hashtbl : Hashtbl.S with type key = t = Hashtbl.Make (Terminal)
end

module rec NonTerminal : sig
  type t = N : 'a nonterminal -> t [@@unboxed]

  include Hashtbl.HashedType with type t := t
  module Hashtbl : Hashtbl.S with type key = t
end = struct
  type t = N : 'a nonterminal -> t [@@unboxed]

  let hash : t -> int = fun (N { id; _ }) -> id
  let equal : t -> t -> bool = ( == )

  module Hashtbl : Hashtbl.S with type key = t = Hashtbl.Make (NonTerminal)
end

module Producers = struct
  type t = P : ('keyword, 'typ, 'action) producers -> t
end

module rec Parameter : sig
  type t = any_parameter

  val equal : t -> t -> bool
end = struct
  type t = any_parameter

  let equal : t -> t -> bool =
   fun (Any pr) (Any pr') ->
    match (pr, pr') with
    | Terminal tk, Terminal tk' -> Terminal.equal (T tk) (T tk')
    | NonTerminal nt, NonTerminal nt' -> NonTerminal.equal (N nt) (N nt')
    | _ -> false
end

let rec iter_entries : type a. (NonTerminal.t -> unit) -> a entries -> unit =
 fun f entries ->
  match entries with
  | [] -> ()
  | nt :: entries ->
      f (N nt);
      iter_entries f entries

let rec fold_entries : type a.
    ('acc -> NonTerminal.t -> 'acc) -> 'acc -> a entries -> 'acc =
 fun f acc entries ->
  match entries with
  | [] -> acc
  | nt :: entries -> fold_entries f (f acc (N nt)) entries

module Automaton : sig
  type t

  val semv : t -> t
  (* Push the top parser semantic value onto the argument stack. *)

  val spos : t -> t
  (* Push the top parser start position onto the argument stack. *)

  val epos : t -> t
  (* Push the top parser end position onto the argument stack. *)

  val pop : t -> t
  (* Pop the top entry from the parser stack. *)

  val call : t -> Obj.t -> t -> t
  (* Call a sub-automaton and push the resulting entry onto the parser stack. *)

  val return : t
  (* Push the semantic value as a new entry onto the parser stack. *)

  val perform :
    t ->
    Obj.t ->
    (int, Obj.t, token) MenhirLib.EngineTypes.env ->
    (int, Obj.t) MenhirLib.EngineTypes.stack
end = struct
  type t =
    | SemanticValue of
        t (* Push the top parser semantic value onto the argument stack. *)
    | StartPosition of t
      (* Push the top parser start position onto the argument stack. *)
    | EndPosition of t
      (* Push the top parser end position onto the argument stack. *)
    | Pop of t (* Pop the top entry from the parser stack. *)
    | Call of t * Obj.t * t
      (* Call a sub-automaton and push the resulting entry onto the parser stack. *)
    | Return (* Push the semantic value as a new entry onto the parser stack. *)

  let semv : t -> t = fun next -> SemanticValue next
  let spos : t -> t = fun next -> StartPosition next
  let epos : t -> t = fun next -> EndPosition next
  let pop : t -> t = fun next -> Pop next

  let call : t -> Obj.t -> t -> t =
   fun automaton value next -> Call (automaton, value, next)

  let return : t = Return

  let rec eval :
      t ->
      Obj.t ->
      Obj.t list ->
      int ->
      Lexing.position ->
      Lexing.position ->
      (int, Obj.t) MenhirLib.EngineTypes.stack ->
      (int, Obj.t) MenhirLib.EngineTypes.stack =
   fun next value args state startp endp stack ->
    match next with
    | SemanticValue next ->
        eval next value (stack.semv :: args) state startp endp stack
    | StartPosition next ->
        eval next value (Obj.repr stack.startp :: args) state startp endp stack
    | EndPosition next ->
        eval next value (Obj.repr stack.endp :: args) state startp endp stack
    | Pop next -> eval next value args stack.state stack.startp endp stack.next
    | Call (inext, ivalue, next) ->
        eval next value args state startp endp
          (eval inext ivalue [] stack.state stack.startp stack.endp stack)
    | Return -> { state; semv = apply value args; startp; endp; next = stack }

  and apply : Obj.t -> Obj.t list -> Obj.t =
   fun value args ->
    match args with
    | [] -> value
    | x :: args -> apply ((Obj.magic value : Obj.t -> Obj.t) x) args

  let perform :
      t ->
      Obj.t ->
      (int, Obj.t, token) MenhirLib.EngineTypes.env ->
      (int, Obj.t) MenhirLib.EngineTypes.stack =
   fun next value { stack; current; _ } ->
    eval next value [] current stack.startp stack.endp stack
end

module Action : sig
  include Backend.ACTION with type token = token

  val none : t
  val init : Obj.t -> t
  (* Push the semantic value as a new entry onto the parser stack. *)

  val peak_semv : t -> t
  (* Push the top parser semantic value onto the argument stack. *)

  val peak_spos : t -> t
  (* Push the top parser start position onto the argument stack. *)

  val peak_epos : t -> t
  (* Push the top parser end position onto the argument stack. *)

  val pop : t -> t
  (* Pop the top entry from the parser stack. *)

  val call : t -> t -> t
  (* Call a sub-automaton and push the resulting entry onto the parser stack. *)
end = struct
  type t = Automaton.t * Obj.t
  type nonrec token = token

  let init : Obj.t -> t = fun semv -> (Automaton.return, semv)
  let none : t = init (Obj.repr ())

  let peak_semv : t -> t =
   fun (automaton, semv) -> (Automaton.semv automaton, semv)

  let peak_spos : t -> t =
   fun (automaton, semv) -> (Automaton.spos automaton, semv)

  let peak_epos : t -> t =
   fun (automaton, semv) -> (Automaton.epos automaton, semv)

  let pop : t -> t = fun (automaton, semv) -> (Automaton.pop automaton, semv)

  let call : t -> t -> t =
   fun (iautomaton, isemv) (automaton, semv) ->
    (Automaton.call iautomaton isemv automaton, semv)

  let perform :
      t ->
      (int, Obj.t, token) MenhirLib.EngineTypes.env ->
      (int, Obj.t) MenhirLib.EngineTypes.stack =
   fun (automaton, semv) -> Automaton.perform automaton semv
end

(** {2 Productions} *)

module Production = struct
  type t = {
    nt : int;
    rhs : (int, int) symbol array;
    action : Action.t;
    positions : ranges;
    prec_decl : (string located * precedence_level) option;
    production_level : production_level;
  }

  let level : int -> production_level option -> production_level =
   fun pos level ->
    match level with
    | Some level -> level
    | None -> ProductionLevel (builtin, pos)

  type builder = int * (int, int) symbol list * Action.t list * int list
end

(* -------------------------------------------------------------------------- *)

module Branch = struct
  type t =
    Producers.t
    * Obj.t
    * (string located * precedence_level) option
    * production_level
    * ranges
end

module Queue = struct
  include Queue

  let transfer_array : 'a t -> 'a array =
   fun q -> Array.init (length q) (fun _ -> Queue.pop q)
end

module State : sig
  type 'a t

  val singleton : 'a -> 'a t
  val inplace_map : ('a -> 'a) -> 'a t -> unit

  val inplace_fork_from_queue :
    ('a -> 'b t -> 'b t) -> 'a Queue.t -> 'b t -> 'b t

  val iter : ('a -> unit) -> 'a t -> unit
end = struct
  type 'a t = Single of { mutable x : 'a } | Parallel of 'a t array

  let singleton : 'a -> 'a t = fun x -> Single { x }

  let rec inplace_map : ('a -> 'a) -> 'a t -> unit =
   fun f s ->
    match s with
    | Single s -> s.x <- f s.x
    | Parallel a -> Array.iter (inplace_map f) a

  let rec copy : 'a t -> 'a t = function
    | Single { x } -> Single { x }
    | Parallel a -> Parallel (Array.map copy a)

  let inplace_fork_from_queue :
      ('a -> 'b t -> 'b t) -> 'a Queue.t -> 'b t -> 'b t =
   fun f q s ->
    let a =
      Array.init (Queue.length q) (fun i -> if i = 0 then s else copy s)
    in
    ignore
      (Queue.fold
         (fun i k ->
           Array.unsafe_set a i (f k (Array.unsafe_get a i));
           i + 1)
         0 q);
    Parallel a

  let rec iter : ('a -> unit) -> 'a t -> unit =
   fun f s ->
    match s with Single { x } -> f x | Parallel a -> Array.iter (iter f) a
end

let interpreter : type a.
    ?debug:bool -> ?compress:bool -> a entries -> rule list -> a interpreter =
 fun ?(debug = false) ?(compress = false) entries rules ->
  let tk_encoding : int Terminal.Hashtbl.t = Terminal.Hashtbl.create 64
  and nt_encoding : int NonTerminal.Hashtbl.t = NonTerminal.Hashtbl.create 128
  and tk_infos : (string * associativity * precedence_level) Queue.t =
    Queue.create ()
  and nt_infos : string Queue.t = Queue.create ()
  and nt_branches : Branch.t Queue.t NonTerminal.Hashtbl.t =
    NonTerminal.Hashtbl.create 128
  and nti_branches : Branch.t Queue.t NonTerminal.Hashtbl.t =
    NonTerminal.Hashtbl.create 32
  in
  let start =
    fold_entries
      (fun n (N { symbol; _ }) ->
        Queue.push (Format.sprintf "%s'%d" symbol n) nt_infos;
        n + 1)
      0 entries
  in
  let push_tk : ?decorate:(int -> string -> string) -> Terminal.t -> unit =
   fun ?(decorate = fun id name -> Format.sprintf "%s'%d" name id)
       (T { symbol; associativity; precedence; _ } as tk) ->
    let id = Queue.length tk_infos in
    Queue.push (decorate id symbol, associativity, precedence) tk_infos;
    Terminal.Hashtbl.add tk_encoding tk id
  in
  let visit_tk : ?decorate:(int -> string -> string) -> Terminal.t -> unit =
   fun ?decorate tk ->
    if tk != T sharp && not (Terminal.Hashtbl.mem tk_encoding tk) then
      push_tk ?decorate tk
  in
  visit_tk ~decorate:(fun _ name -> name) (T error);
  let push_nt : NonTerminal.t -> unit =
   fun (N { symbol; _ } as nt) ->
    let id = Queue.length nt_infos in
    Queue.push (Format.sprintf "%s'%d" symbol id) nt_infos;
    NonTerminal.Hashtbl.add nt_encoding nt id
  in
  let visit_nt : NonTerminal.t -> unit =
   fun (N { inline; _ } as nt) ->
    if (not inline) && not (NonTerminal.Hashtbl.mem nt_encoding nt) then
      push_nt nt
  in
  let visit_param : Parameter.t -> unit =
   fun (Any param) ->
    match param with
    | Terminal tk -> visit_tk (T tk)
    | NonTerminal nt -> visit_nt (N nt)
  in
  let rec visit_producers : Producers.t -> unit = function
    | P producers -> (
        match producers with
        | Ignore (pr, producers) ->
            visit_param (Any pr);
            visit_producers (P producers)
        | Bind (pr, producers) ->
            visit_param (Any pr);
            visit_producers (P producers)
        | Return -> ()
        | Before producers -> visit_producers (P producers)
        | Start producers -> visit_producers (P producers)
        | End producers -> visit_producers (P producers)
        | pr :: producers ->
            visit_param (Any pr);
            visit_producers (P producers)
        | [] -> ())
  in
  let add_branch : type a.
      a Queue.t NonTerminal.Hashtbl.t -> NonTerminal.t -> a -> unit =
   fun tbl nt elt ->
    Queue.add elt
      (try NonTerminal.Hashtbl.find tbl nt
       with Not_found ->
         let q = Queue.create () in
         NonTerminal.Hashtbl.add tbl nt q;
         q)
  in
  List.iteri
    (fun i
         (Rule
            (({ inline; _ } as nt), producers, action, level, precedence, source))
       ->
      visit_nt (N nt);
      visit_producers (P producers);
      add_branch
        (if inline then nti_branches else nt_branches)
        (N nt)
        ( P producers,
          Obj.repr action,
          precedence,
          Production.level i level,
          Option.to_list source ))
    rules;
  push_tk ~decorate:(fun _ name -> name) (T sharp);
  let productions : Production.t Queue.t = Queue.create ()
  and nt_productions : (int * int) array =
    Array.make (Queue.length nt_infos) (-1, -1)
  in
  ignore
    (fold_entries
       (fun i (N { inline; _ } as nt) ->
         if inline then (
           push_nt nt;
           NonTerminal.Hashtbl.add nt_branches nt
             (NonTerminal.Hashtbl.find nti_branches nt));
         Queue.push
           Production.
             {
               nt = i;
               rhs = [| N (NonTerminal.Hashtbl.find nt_encoding nt) |];
               action = Action.none;
               positions = [];
               prec_decl = None;
               production_level = ProductionLevel (builtin, i);
             }
           productions;
         Array.unsafe_set nt_productions i (i, i + 1);
         i + 1)
       0 entries);
  let pop : Production.builder State.t -> Production.builder State.t =
   fun acc ->
    State.inplace_map
      (fun (size, rev_production, stack, inlined) ->
        ( size,
          rev_production,
          List.cons (Action.pop (List.hd stack)) (List.tl stack),
          inlined ))
      acc;
    acc
  in
  let rec resolve_producers :
      ?sep:(Production.builder State.t -> Production.builder State.t) ->
      Producers.t ->
      Production.builder State.t ->
      Production.builder State.t =
   fun ?(sep = pop) (P producers) acc ->
    match producers with
    | Ignore (pr, producers) ->
        resolve_producers ~sep:pop (P producers)
          (resolve_parameter ~bind:false (Any pr) (sep acc))
    | Bind (pr, producers) ->
        resolve_producers ~sep:pop (P producers)
          (resolve_parameter ~bind:true (Any pr) (sep acc))
    | Before producers ->
        resolve_producers ~sep:Fun.id (P producers)
          (resolve_position ~start:false acc)
    | Start producers ->
        resolve_producers ~sep:Fun.id (P producers)
          (resolve_position ~start:true (pop acc))
    | End producers ->
        resolve_producers ~sep (P producers) (resolve_position ~start:false acc)
    | Return -> acc
    | pr :: producers ->
        resolve_producers ~sep:pop (P producers)
          (resolve_parameter ~bind:true (Any pr) (sep acc))
    | [] -> acc
  and resolve_nonterminal :
      bind:bool ->
      NonTerminal.t ->
      Production.builder State.t ->
      Production.builder State.t =
   fun ~bind (N { id; inline; _ } as nt) acc ->
    if inline then
      State.inplace_fork_from_queue
        (fun ((producers, action, _, _, _) : Branch.t) acc ->
          State.inplace_map
            (fun (size, rev_production, stack, inlined) ->
              if List.memq id inlined then
                raise (Invalid_argument "recusrsive nonterminal inlining");
              let stack =
                if bind then
                  List.cons (Action.peak_semv (List.hd stack)) (List.tl stack)
                else stack
              in
              ( size,
                rev_production,
                List.cons (Action.init action) stack,
                List.cons id inlined ))
            acc;
          let acc = resolve_producers producers acc in
          State.inplace_map
            (fun (size, rev_production, stack, inlined) ->
              ( size,
                rev_production,
                List.cons
                  (Action.call (List.hd stack) (List.hd (List.tl stack)))
                  (List.tl (List.tl stack)),
                List.tl inlined ))
            acc;
          acc)
        (NonTerminal.Hashtbl.find nti_branches nt)
        acc
    else
      let symbol = N (NonTerminal.Hashtbl.find nt_encoding nt) in
      State.inplace_map
        (fun (size, rev_production, stack, inlined) ->
          ( size + 1,
            List.cons symbol rev_production,
            (if bind then
               List.cons (Action.peak_semv (List.hd stack)) (List.tl stack)
             else stack),
            inlined ))
        acc;
      acc
  and resolve_parameter :
      bind:bool ->
      Parameter.t ->
      Production.builder State.t ->
      Production.builder State.t =
   fun ~bind (Any param) acc ->
    match param with
    | Terminal tk ->
        State.inplace_map
          (fun (size, rev_production, stack, inlined) ->
            ( size + 1,
              List.cons
                (T (Terminal.Hashtbl.find tk_encoding (T tk)))
                rev_production,
              (if bind then
                 List.cons (Action.peak_semv (List.hd stack)) (List.tl stack)
               else stack),
              inlined ))
          acc;
        acc
    | NonTerminal nt -> resolve_nonterminal ~bind (N nt) acc
  and resolve_position :
      start:bool -> Production.builder State.t -> Production.builder State.t =
   fun ~start acc ->
    let opcode : Action.t -> Action.t =
      if start then Action.peak_spos else Action.peak_epos
    in
    State.inplace_map
      (fun (size, rev_production, stack, inlined) ->
        ( size,
          rev_production,
          List.cons (opcode (List.hd stack)) (List.tl stack),
          inlined ))
      acc;
    acc
  in
  NonTerminal.Hashtbl.iter
    (fun (N nt) branches ->
      let encoding = NonTerminal.Hashtbl.find nt_encoding (N nt) in
      let start_index = Queue.length productions in
      Queue.iter
        (fun (producers, action, precedence_level, production_level, positions)
           ->
          State.iter
            (fun (size, rev_production, action, _) ->
              let rhs = Array.make size (T 0) in
              List.iteri
                (fun i symbol -> Array.unsafe_set rhs (size - 1 - i) symbol)
                rev_production;
              Queue.push
                ({
                   nt = encoding;
                   rhs;
                   action = List.hd action;
                   positions;
                   prec_decl = precedence_level;
                   production_level;
                 }
                  : Production.t)
                productions)
            (resolve_producers producers
               (State.singleton
                  ((0, [], [ Action.init action ], []) : Production.builder))))
        branches;
      let end_index = Queue.length productions in
      Array.unsafe_set nt_productions encoding (start_index, end_index))
    nt_branches;
  let module G0 :
    MenhirMrk.GrammarConstruction.G
      with type ocamltype = unit
       and type merge_fun = unit
       and type attributes = unit
       and type action = Action.t = struct
    type ocamltype = unit
    type merge_fun = unit
    type attributes = unit
    type action = Action.t

    (* -------------------------------------------------------------------------- *)

    (* Nonterminal symbols. *)

    module Nonterminal = struct
      type t = int

      let names : string array = Queue.transfer_array nt_infos
      let n : int = Array.length names

      include Base.Fix.Numbering.OperationsForIntSegment (struct
        let n = n
      end)

      let[@inline] iterx f = Base.MInt.iterij start n f (* DUMMY *)
      let[@inline] foldx f accu = Base.MInt.foldij start n f accu (* DUMMY *)
      let[@inline] mapx f = Base.MInt.mapij start n f (* DUMMY *)
      let is_internal_start nt = nt < start (* DUMMY *)
      let is_user_start _ = assert false (* DUMMY *)
      let lookup _ = assert false (* DUMMY *)
      let ocamltype _ = None (* DUMMY *)
      let ocamltype_of_start_symbol _ = () (* DUMMY *)
      let check_every_symbol_has_ocaml_type _ = () (* DUMMY *)
      let positions _ : ranges = [] (* DUMMY *)
      let attributes _ = () (* DUMMY *)
      let merge_function _ = None (* DUMMY *)
      let user_start_symbols : t list = [] (* DUMMY *)
      let print : bool -> t -> string = fun _ nt -> Array.unsafe_get names nt
    end
    (* Nonterminal *)

    (* -------------------------------------------------------------------------- *)

    (* Terminal symbols. *)

    module Terminal = struct
      type t = int

      let infos : (string * associativity * precedence_level) array =
        Queue.transfer_array tk_infos

      let n : int = Array.length infos

      include Base.Fix.Numbering.OperationsForIntSegment (struct
        let n = n
      end)

      let print : t -> string =
       fun tk ->
        let symbol, _, _ = Array.unsafe_get infos tk in
        symbol

      let sharp : t = Terminal.Hashtbl.find tk_encoding (T sharp)
      let error : t = Terminal.Hashtbl.find tk_encoding (T error)

      let associativity : t -> associativity =
       fun tk ->
        let _, associativity, _ = Array.unsafe_get infos tk in
        associativity

      let () = assert (sharp = n - 1)
      let[@inline] iterx f = Base.MInt.iteri sharp f
      let[@inline] foldx f accu = Base.MInt.foldi sharp f accu
      let[@inline] mapx f = Base.MInt.mapi sharp f
      let[@inline] initx f = Base.MInt.initi sharp f
      let () = assert (error = 0)
      let[@inline] iter_real f = Base.MInt.iterij 1 sharp f
      let[@inline] fold_real f accu = Base.MInt.foldij 1 sharp f accu
      let[@inline] map_real f = Base.MInt.mapij 1 sharp f

      let precedence : t -> precedence_level =
       fun tk ->
        let _, _, precedence = Array.unsafe_get infos tk in
        precedence

      let lookup _ = assert false (* DUMMY *)
      let special _ = assert false (* DUMMY *)
      let real _ = assert false (* DUMMY *)
      let eof = None (* DUMMY *)
      let ocamltype _ = None (* DUMMY *)
      let attributes _ = () (* DUMMY *)
      let alias _ = None (* DUMMY *)
      let unquoted_alias _ = None (* DUMMY *)
      let every_token_has_an_alias = false (* DUMMY *)
    end
    (* Terminal *)

    (* -------------------------------------------------------------------------- *)

    (* Productions. *)

    module Production = struct
      type t = int

      let infos : Production.t array = Queue.transfer_array productions
      let n : int = Array.length infos

      include Base.Fix.Numbering.OperationsForIntSegment (struct
        let n = n
      end)

      let start : int = start

      (* Accessors. *)

      let[@inline] nt prod =
        match Array.unsafe_get infos prod with { nt; _ } -> nt

      let[@inline] rhs prod =
        match Array.unsafe_get infos prod with { rhs; _ } -> rhs

      let[@inline] length prod = Array.length (rhs prod)

      let action prod =
        match Array.unsafe_get infos prod with { action; _ } -> action

      let[@inline] positions prod =
        match Array.unsafe_get infos prod with { positions; _ } -> positions

      let[@inline] prec_decl prod =
        match Array.unsafe_get infos prod with { prec_decl; _ } -> prec_decl

      let[@inline] production_level prod =
        match Array.unsafe_get infos prod with
        | { production_level; _ } -> production_level

      let[@inline] is_start : t -> bool = fun prod -> prod < start

      let get_start prod =
        assert (is_start prod);
        match (rhs prod).(0) with N nt -> nt | T _ -> assert false

      (* Printing a production. *)

      let print prod =
        (* assert (not (is_start prod)); *)
        let nt = nt prod and rhs = rhs prod in
        let nt = Nonterminal.print false nt in
        if Array.length rhs = 0 then
          (* Avoid producing a trailing space. *)
          Printf.sprintf "%s ->" nt
        else
          Printf.sprintf "%s -> %s" nt
            (Base.MString.subarray_bullet
               (fun symbol ->
                 match symbol with
                 | T tk -> Terminal.print tk
                 | N nt -> Nonterminal.print false nt)
               " " 0 (-1) "." rhs)

      (* Iteration. *)

      let[@inline] iterx f = Base.MInt.iterij start n f
      let[@inline] foldx f accu = Base.MInt.foldij start n f accu
      let[@inline] mapx f = Base.MInt.mapij start n f

      let[@inline] iternt nt f =
        let k, k' = Array.unsafe_get nt_productions nt in
        Base.MInt.iterij k k' f

      let[@inline] foldnt nt f accu =
        let k, k' = Array.unsafe_get nt_productions nt in
        Base.MInt.foldij k k' f accu

      let[@inline] mapnt nt f =
        let k, k' = Array.unsafe_get nt_productions nt in
        Base.MInt.mapij k k' f

      let[@inline] productions nt =
        let k, k' = Array.unsafe_get nt_productions nt in
        Base.MInt.interval k k'

      let attributes _ = () (* DUMMY *)
      let start_production _ = assert false (* DUMMY *)
      let def _ = assert false (* DUMMY *)
      let identifiers _ = assert false (* DUMMY *)
      let rhs_attributes _ = assert false (* DUMMY *)
      let error_free _ = assert false (* DUMMY *)

      let test_start prod =
        if is_start prod then Some (get_start prod) else None

      let describe gerund prod =
        match test_start prod with
        | Some nt ->
            let ending = if gerund then "ing" else "" in
            Printf.sprintf "accept%s %s" ending (Nonterminal.print false nt)
        | None ->
            let ending = if gerund then "ing" else "e" in
            Printf.sprintf "reduc%s production %s" ending (print prod)
    end
    (* Production *)

    let attributes = ()

    let on_error_reduce = Base.StringMap.empty

    (* -------------------------------------------------------------------------- *)
  end in
  let module Grammar = MenhirMrk.GrammarConstruction.Make (G0) () in
  let module Lr0 = MenhirMrk.LR0Construction.Make (Grammar) in
  let module A0 =
    MenhirMrk.LR1Construction.Make
      (Lr0)
      (struct
        let construction_mode = `LALR
      end)
      ()
  in
  let module Precedence = MenhirMrk.Precedence.Make (Grammar) in
  let module A1 = MenhirMrk.SilentConflictResolution.Run (A0) (Precedence) in
  let module A2 = MenhirMrk.Renumber.Run (A1) in
  let module A3 =
    MenhirMrk.SevereConflictResolution.Run (A2) (Precedence)
      (struct
        let main = Base.Report.null
      end)
  in
  let module A4 = MenhirMrk.DefaultReductionIntroduction.Run (A3) in
  let module Final = struct
    include A4
    include Base.Fix.Numbering.Operations (A4)

    let start_node prod = Lr0.G.ProductionMap.find prod entry

    let has_default_reduction node =
      match test_default_reduction node with Some _ -> true | None -> false

    let has_default_reduction_on_sharp node =
      match test_default_reduction node with
      | Some (_, toks) when Lr0.G.TerminalSet.mem Lr0.G.Terminal.sharp toks ->
          assert (Lr0.G.TerminalSet.cardinal toks = 1);
          true
      | _ -> false
  end in
  let module Table =
    Backend.Make
      (struct
        type t = token

        let encode : t -> int =
         fun (Token (tk, _)) -> Terminal.Hashtbl.find tk_encoding (T tk)

        let value : t -> Obj.t = fun (Token (_, a)) -> Obj.repr a
      end)
      (Final)
      (Action)
      (struct
        let debug = debug
        let compression = compress
      end)
  in
  let module Interpreter = MenhirLib.TableInterpreter.MakeEngineTable (Table) in
  let module Engine = MenhirLib.Engine.Make (Interpreter) in
  (module struct
    include Engine

    type 'a start = Lexing.position -> 'a checkpoint

    external unwrap : Obj.t start -> 'a start = "%identity"

    type mapper = {
      bind : 'a. int -> 'a nonterminal -> Lexing.position -> 'a checkpoint;
    }

    type 'a starts =
      | ( :: ) :
          (Lexing.position -> 'a checkpoint) * 'b starts
          -> ('a -> 'b) starts
      | [] : unit starts

    let starts : a starts =
      let[@tail_mod_cons] rec mapi : type a.
          mapper -> int -> a entries -> a starts =
       fun mapper i entries ->
        match entries with
        | [] -> []
        | nt :: entries ->
            mapper.bind i nt :: (mapi [@tailcall]) mapper (i + 1) entries
      in
      let bind : type a. int -> a nonterminal -> Lexing.position -> a checkpoint
          =
       fun prod _ ->
        unwrap
          (Engine.start
             (Final.encode
                (Final.start_node (Final.Lr0.G.Production.decode prod))))
      in
      mapi { bind } 0 entries

    type nonrec a = a
  end)
