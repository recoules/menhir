open Syntax

let ocaml_ident : string -> string =
  String.map (function
    | ('\'' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z') as c -> c
    | _ -> '_')

type partial_grammar = {
  filenames : string list;
  preludes : string located list;
  postludes : string located list;
  parameters : string located list;
  (* start_symbols : range StringMap.t; *)
  types : ocamltype StringMap.t;
  prec_annotations : precedence_level StringMap.t;
  imported_tokens : properties StringMap.t;
  imported_nonterms : StringSet.t;
  exported_tokens : properties StringMap.t;
  exported_rules : parameterized_rule StringMap.t;
}

let keep : filenames:string list -> 'a located -> bool =
 fun ~filenames located ->
  List.mem (Range.filename (Located.position located)) filenames

let is_expanded : string -> bool = Fun.flip String.contains '('

let slice_rule :
    filenames:string list ->
    _ ->
    parameterized_rule ->
    parameterized_rule option =
 fun ~filenames _
     {
       pr_public;
       pr_inline;
       pr_nt;
       pr_positions;
       pr_attributes;
       pr_parameters;
       pr_branches;
       pr_merge;
     } ->
  if is_expanded pr_nt then None
  else
    match
      List.filter
        (fun position -> List.mem (Range.filename position) filenames)
        pr_positions
    with
    | [] -> None
    | pr_positions ->
        Some
          {
            pr_public;
            pr_inline;
            pr_nt;
            pr_positions;
            pr_attributes;
            pr_parameters;
            pr_branches =
              List.filter
                (fun { pb_position; _ } ->
                  List.mem (Range.filename pb_position) filenames)
                pr_branches;
            pr_merge;
          }

let (* rec *) fold_parameter : ('a -> symbol -> 'a) -> 'a -> parameter -> 'a =
 fun f acc parameter ->
  match parameter with
  | ParamVar located_symbol -> f acc (Located.value located_symbol)
  | ParamApp _ | ParamAnonymous _ -> assert false

let slice : filenames:string list -> grammar -> partial_grammar =
  let scan_symbols :
      string Queue.t -> parameterized_branch list -> StringSet.t -> StringSet.t
      =
   fun expanded_symbols branches acc ->
    List.fold_left
      (fun acc { pb_producers; _ } ->
        List.fold_left
          (fun acc (_, parameter, _) ->
            fold_parameter
              (fun symbols symbol ->
                if is_expanded symbol && not (StringSet.mem symbol symbols) then
                  Queue.add symbol expanded_symbols;
                StringSet.add symbol symbols)
              acc parameter)
          acc pb_producers)
      acc branches
  in
  let rec fixpoint :
      rules:parameterized_rule StringMap.t ->
      string Queue.t ->
      StringSet.t ->
      parameterized_rule StringMap.t ->
      StringSet.t * parameterized_rule StringMap.t =
   fun ~rules worklist all_symbols exported_rules ->
    if Queue.is_empty worklist then (all_symbols, exported_rules)
    else
      let expanded_symbol = Queue.pop worklist in
      let ({ pr_branches } as rule) = StringMap.find expanded_symbol rules in
      fixpoint ~rules worklist
        (scan_symbols worklist pr_branches all_symbols)
        (StringMap.add expanded_symbol
           { rule with pr_public = false }
           exported_rules)
  in
  fun ~filenames
    {
      p_preludes;
      p_postludes;
      p_parameters;
      p_start_symbols;
      p_types = _;
      p_tokens;
      p_rules;
    }
  ->
    let start_symbols =
      StringMap.filter
        (fun _ position -> List.mem (Range.filename position) filenames)
        p_start_symbols
    in
    let exported_rules = StringMap.filter_map (slice_rule ~filenames) p_rules in
    let expanded_symbols = Queue.create () in
    let all_symbols =
      StringMap.fold
        (fun _ { pr_branches; _ } acc ->
          scan_symbols expanded_symbols pr_branches acc)
        exported_rules StringSet.empty
    in
    let all_symbols, exported_rules =
      fixpoint ~rules:p_rules expanded_symbols all_symbols exported_rules
    in
    let all_tokens, all_nonterms =
      StringSet.partition
        (fun symbol ->
          StringMap.mem symbol p_tokens || String.equal symbol "error")
        all_symbols
    in
    let imported_tokens =
      StringMap.filter (fun token _ -> StringSet.mem token all_tokens) p_tokens
    and exported_tokens =
      StringMap.filter
        (fun _ { tk_position } ->
          List.mem (Range.filename tk_position) filenames)
        p_tokens
    and imported_nonterms =
      StringSet.filter
        (fun symbol ->
          try
            let { pr_public; pr_nt; _ } = StringMap.find symbol p_rules in
            pr_public && not (is_expanded pr_nt)
          with Not_found ->
            Format.eprintf "!!!! %s" symbol;
            raise Not_found)
        all_nonterms
    and exported_nonterms =
      StringMap.fold
        (fun _ { pr_nt; pr_public; _ } symbols ->
          if pr_public || StringMap.mem pr_nt start_symbols then
            StringSet.add pr_nt symbols
          else symbols)
        exported_rules StringSet.empty
    in
    let prec_annotations =
      StringMap.fold
        (fun _ { pr_branches; _ } acc ->
          List.fold_left
            (fun acc { pb_prec_annotation; _ } ->
              match pb_prec_annotation with
              | None -> acc
              | Some located_symbol ->
                  let symbol = Located.value located_symbol in
                  let { tk_precedence; _ } = StringMap.find symbol p_tokens in
                  StringMap.add symbol tk_precedence acc)
            acc pr_branches)
        exported_rules StringMap.empty
    in
    let filenames =
      StringSet.elements
        (StringMap.fold
           (fun _ { pr_positions; _ } acc ->
             List.fold_left
               (fun acc range -> StringSet.add (Range.filename range) acc)
               acc pr_positions)
           exported_rules StringSet.empty)
    in
    {
      filenames;
      preludes = List.filter (keep ~filenames) p_preludes;
      postludes = List.filter (keep ~filenames) p_postludes;
      parameters = List.filter (keep ~filenames) p_parameters;
      types = StringMap.empty;
      prec_annotations;
      imported_tokens;
      imported_nonterms = StringSet.union imported_nonterms exported_nonterms;
      exported_tokens;
      exported_rules;
    }

let infer : partial_grammar -> ocamltype StringMap.t -> partial_grammar =
 fun pg types -> { pg with types }

let runtimelib = "MenhirMrk"
let staticVersion = "MenhirLib.StaticVersion"
let runtime = "Cromlech"
let runtime_grammar = runtime ^ ".Grammar"
let terminal_typ : string = runtime_grammar ^ ".terminal"
let nonterminal_typ : string = runtime_grammar ^ ".nonterminal"
let terminal_fun : IL.expr = EVar (runtime_grammar ^ ".terminal")
let nonterminal_fun : IL.expr = EVar (runtime_grammar ^ ".nonterminal")
let precedence_typ : string = runtimelib ^ ".MiddleAPI.precedence_level"
let located_typ : string = runtimelib ^ ".MiddleAPI.located"
let locate_fun : IL.expr = EVar (runtimelib ^ ".MiddleAPI.locate")
let rule_typ : string = runtime_grammar ^ ".rule"
let rule_fun : IL.expr = EVar (runtime_grammar ^ ".rule")
let rule_list_typ : IL.typ = TypApp ("list", [ TypTextual (Inferred rule_typ) ])
let cons_fun : IL.expr = EVar "List.cons"
let production_level_typ : string = runtimelib ^ ".MiddleAPI.production_level"
let error_token : IL.expr = EVar (runtime_grammar ^ ".error")

let prec_annot_typ : IL.typ =
  TypApp
    ( "option",
      [
        TypTuple
          [
            TypApp (located_typ, [ TypTextual (Inferred "string") ]);
            TypTextual (Inferred precedence_typ);
          ];
      ] )

let associativity_expr : associativity -> IL.expr =
  let eleft : IL.expr = EData ("LeftAssoc", [])
  and eright : IL.expr = EData ("RightAssoc", [])
  and enon : IL.expr = EData ("NonAssoc", [])
  and eundef : IL.expr = EData ("UndefinedAssoc", []) in
  function
  | LeftAssoc -> eleft
  | RightAssoc -> eright
  | NonAssoc -> enon
  | UndefinedAssoc -> eundef

let position_expr : Lexing.position -> IL.expr =
 fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
  ERecord
    [
      ("Lexing.pos_fname", EStringConst pos_fname);
      ("pos_lnum", EIntConst pos_lnum);
      ("pos_bol", EIntConst pos_bol);
      ("pos_cnum", EIntConst pos_cnum);
    ]

let range_expr : range -> IL.expr =
 fun pos ->
  ETuple [ position_expr (Range.startp pos); position_expr (Range.endp pos) ]

let precedence_lookup : string -> int -> string =
 fun filename level -> Format.sprintf "%s_level%d" (ocaml_ident filename) level

let precedence_expr : precedence_level -> IL.expr =
  let eundef : IL.expr = EData ("UndefinedPrecedence", []) in
  function
  | UndefinedPrecedence -> eundef
  | PrecedenceLevel located_level ->
      let file, level = Located.value located_level in
      EData ("PrecedenceLevel", [ EVar (precedence_lookup file level) ])

let production_level_expr : production_level -> IL.expr = function
  | ProductionLevel (file, level) ->
      EData ("ProductionLevel", [ EVar (ocaml_ident file); EIntConst level ])

let unit_typ : IL.typ = TypTextual (Inferred "unit")

module Run (G : sig
  val grammar : partial_grammar
end) =
struct
  open ILConstruction
  open IL

  let start_time = Time.start ()

  let versiondef =
    pdef PUnit @@ EVar (staticVersion ^ ".require_" ^ Version.version)

  let app1scheme : typ -> string -> typescheme =
   fun t name ->
    { quantifiers = []; locally_abstract = false; body = TypApp (name, [ t ]) }

  let token_type : ocamltype option -> IL.typ = function
    | None -> unit_typ
    | Some otyp -> TypTextual otyp

  let token_name : string -> string =
   fun name ->
    Format.sprintf "tk_%s" (String.lowercase_ascii (ocaml_ident name))

  let nonter_name : string -> string =
   fun name ->
    Format.sprintf "nt_%s" (String.lowercase_ascii (ocaml_ident name))

  let prec_expr : prec_annotation -> IL.expr =
    let enone = EData ("None", []) in
    function
    | None -> enone
    | Some located_symbol ->
        let symbol = Located.value located_symbol
        and position = Located.position located_symbol in
        EData
          ( "Some",
            [
              ETuple
                [
                  EApp (locate_fun, [ range_expr position; EStringConst symbol ]);
                  precedence_expr
                    (StringMap.find symbol G.grammar.prec_annotations);
                ];
            ] )

  module KeywordExpansion = KeywordExpansion.Lib (struct
    type nonrec producer = producer
    type producers = producer list

    let prod_id (located_symbol, _, _) = Located.value located_symbol

    let prod_symbol (_, param, _) =
      match param with
      | ParamVar located_symbol -> Located.value located_symbol
      | ParamApp _ -> failwith "parametrized app"
      | ParamAnonymous _ -> assert false

    let nullable _ = true
    let epsilon _ = false
  end)

  let semvars : action -> StringSet.t =
    let rec search_forward_inner :
        string -> int -> int -> string -> int -> int -> bool =
     fun str lstr i pat lpat j ->
      j = lpat
      ||
      if String.get str (i + j) <> String.get pat j then
        search_forward_outer str lstr (i + 1) pat lpat
      else search_forward_inner str lstr i pat lpat (j + 1)
    and search_forward_outer : string -> int -> int -> string -> int -> bool =
     fun str lstr i pat lpat ->
      lpat + i < lstr && search_forward_inner str lstr i pat lpat 0
    in
    let search_forward : string -> string -> bool =
     fun str pat ->
      search_forward_outer str (String.length str) 0 pat (String.length pat)
    in
    let rec emem : string -> expr -> bool =
     fun var exp ->
      match exp with
      | EVar var' -> String.equal var var'
      | EData (_, exps) | ETuple exps -> List.exists (emem var) exps
      | ETextual code -> search_forward (Located.value code) var
      | _ -> true (* Overapproximation *)
    in
    fun action ->
      StringSet.filter
        (Fun.flip emem (Action.expr action))
        (Action.semvars action)

  let branch_expr : string -> parameterized_branch -> IL.expr =
    let ereturn = EData ("Return", []) in
    fun symbol
      {
        pb_position;
        pb_production_level;
        pb_prec_annotation;
        pb_producers;
        pb_action;
        _;
      }
    ->
      let source = symbol ^ "__source"
      and level = symbol ^ "__level"
      and precedence = symbol ^ "__precedence" in
      let semvars = semvars pb_action in
      let pb_action = KeywordExpansion.expand_action pb_producers pb_action in
      let has_beforep, has_startp, has_endp =
        Keyword.KeywordSet.fold
          (fun keyword (has_beforep, has_startp, has_endp) ->
            match keyword with
            | Position (Before, WhereEnd, _) -> (true, has_startp, has_endp)
            | Position (RightNamed symbol, WhereStart, FlavorPosition) ->
                (has_beforep, StringSet.add symbol has_startp, has_endp)
            | Position (RightNamed symbol, WhereEnd, FlavorPosition) ->
                (has_beforep, has_startp, StringSet.add symbol has_endp)
            | Position _ -> assert false)
          (Action.keywords pb_action)
          (false, StringSet.empty, StringSet.empty)
      in
      let patterns =
        List.fold_left
          (fun acc (located_id, _, _) ->
            let id = Located.value located_id in
            let acc =
              if StringSet.mem id has_endp then
                PVar (Keyword.posvar (RightNamed id) WhereEnd FlavorPosition)
                :: acc
              else acc
            in
            let acc =
              if StringSet.mem id semvars then PVar id :: acc else acc
            in
            if StringSet.mem id has_startp then
              PVar (Keyword.posvar (RightNamed id) WhereStart FlavorPosition)
              :: acc
            else acc)
          [] (List.rev pb_producers)
      in
      let patterns =
        if has_beforep then
          PVar (Keyword.posvar Before WhereEnd FlavorPosition) :: patterns
        else patterns
      in
      let producers =
        List.fold_left
          (fun acc (located_id, param, _) ->
            let id = Located.value located_id in
            let acc =
              if StringSet.mem id has_endp then EData ("End", [ acc ]) else acc
            in
            let const = if StringSet.mem id semvars then "Bind" else "Ignore" in
            let acc =
              match param with
              | ParamVar located_symbol ->
                  let symbol = Located.value located_symbol in
                  let param =
                    if String.equal symbol "error" then
                      EData ("Terminal", [ error_token ])
                    else if StringMap.mem symbol G.grammar.imported_tokens then
                      EData ("Terminal", [ EVar (token_name symbol) ])
                    else EData ("NonTerminal", [ EVar (nonter_name symbol) ])
                  in
                  EData (const, [ param; acc ])
              | ParamApp _ -> failwith "not yet implemented"
              | ParamAnonymous _ -> assert false
            in
            if StringSet.mem id has_startp then EData ("Start", [ acc ])
            else acc)
          ereturn (List.rev pb_producers)
      in
      let producers =
        if has_beforep then EData ("Before", [ producers ]) else producers
      in
      let action = Action.expr pb_action in
      ELet
        ( [
            (PVar source, range_expr pb_position);
            ( PVar level,
              EAnnot
                ( production_level_expr pb_production_level,
                  type2scheme (TypTextual (Inferred production_level_typ)) ) );
            ( PVar precedence,
              EAnnot (prec_expr pb_prec_annotation, type2scheme prec_annot_typ)
            );
          ],
          EApp
            ( rule_fun,
              [
                ETextual (Located.locate Range.dummy ("~source:" ^ source));
                ETextual (Located.locate Range.dummy ("~level:" ^ level));
                ETextual
                  (Located.locate Range.dummy ("?precedence:" ^ precedence));
                EVar symbol;
                producers;
                (if patterns = [] then action else EFun (patterns, action));
              ] ) )

  let rev_branch_expr : parameterized_rule -> IL.expr list =
   fun { pr_nt; pr_parameters; pr_branches; _ } ->
    if pr_parameters = [] then
      List.rev_map (branch_expr (nonter_name pr_nt)) pr_branches
    else failwith (Format.sprintf "parametrized nonterminal %s" pr_nt)

  let exportvaldefs =
    StringMap.fold
      (fun tk_name { tk_ocamltype; _ } valdefs ->
        (token_name tk_name, app1scheme (token_type tk_ocamltype) terminal_typ)
        :: valdefs)
      G.grammar.imported_tokens
      (StringSet.fold
         (fun pr_nt valdefs ->
           ( nonter_name pr_nt,
             app1scheme
               (TypTextual (StringMap.find pr_nt G.grammar.types))
               nonterminal_typ )
           :: valdefs)
         G.grammar.imported_nonterms [])

  let tk_def : string -> properties -> valdef =
   fun name { tk_position; tk_ocamltype; tk_associativity; tk_precedence; _ } ->
    let symbol = token_name name in
    let position = symbol ^ "__position" in
    {
      attribute = "";
      valpat = PVar symbol;
      valval =
        EAnnot
          ( ELet
              ( [ (PVar position, range_expr tk_position) ],
                EApp
                  ( terminal_fun,
                    [
                      ETextual
                        (Located.locate Range.dummy ("~position:" ^ position));
                      EStringConst name;
                      associativity_expr tk_associativity;
                      precedence_expr tk_precedence;
                    ] ) ),
            app1scheme (token_type tk_ocamltype) terminal_typ );
    }

  let nt_def : inline:bool -> string -> valdef =
   fun ~inline name ->
    {
      attribute = "";
      valpat = PVar (nonter_name name);
      valval =
        EAnnot
          ( EApp
              ( nonterminal_fun,
                [
                  ETextual
                    (Located.locate Range.dummy
                       (Format.sprintf "~inline:%B" inline));
                  EStringConst name;
                ] ),
            app1scheme
              (TypTextual (StringMap.find name G.grammar.types))
              nonterminal_typ );
    }

  let filedefs =
    List.map
      (fun file ->
        {
          attribute = "";
          valpat = PVar (ocaml_ident file);
          valval = EStringConst file;
        })
      G.grammar.filenames

  let precedencedefs =
    List.rev
      (StringMap.fold
         (fun filename levels acc ->
           IntMap.Unbounded.fold
             (fun _ located_level acc ->
               let file, level = Located.value located_level
               and pos = Located.position located_level in
               {
                 attribute = "";
                 valpat = PVar (precedence_lookup filename level);
                 valval =
                   EAnnot
                     ( EApp
                         ( locate_fun,
                           [
                             range_expr pos;
                             ETuple [ EVar (ocaml_ident file); EIntConst level ];
                           ] ),
                       app1scheme
                         (TypTuple
                            [
                              TypTextual (Inferred "string");
                              TypTextual (Inferred "int");
                            ])
                         located_typ );
               }
               :: acc)
             levels acc)
         (StringMap.fold
            (fun _ { tk_precedence; _ } acc ->
              match tk_precedence with
              | UndefinedPrecedence -> acc
              | PrecedenceLevel located_level ->
                  let file, level = Located.value located_level in
                  StringMap.update file
                    (function
                      | None ->
                          Some (IntMap.Unbounded.singleton level located_level)
                      | Some map ->
                          Some (IntMap.Unbounded.add level located_level map))
                    acc)
            G.grammar.exported_tokens StringMap.empty)
         [])

  let exportdefs =
    StringMap.fold
      (fun tk_name tk_props valdefs ->
        match StringMap.find tk_name G.grammar.exported_tokens with
        | { tk_is_declared = true; _ } -> tk_def tk_name tk_props :: valdefs
        | { tk_is_declared = false; _ } | (exception Not_found) -> valdefs)
      G.grammar.exported_tokens
      (StringMap.fold
         (fun _ { pr_nt; pr_inline; _ } letdefs ->
           if StringSet.mem pr_nt G.grammar.imported_nonterms then
             nt_def ~inline:pr_inline pr_nt :: letdefs
           else letdefs)
         G.grammar.exported_rules [])

  let localdefs =
    StringMap.fold
      (fun _ { pr_nt; pr_inline; _ } letdefs ->
        if StringSet.mem pr_nt G.grammar.imported_nonterms then letdefs
        else nt_def ~inline:pr_inline pr_nt :: letdefs)
      G.grammar.exported_rules []

  let extend_typ = TypArrow (rule_list_typ, rule_list_typ)

  let extenddef =
    {
      attribute = "";
      valpat = PVar "extend";
      valval =
        EAnnot
          ( EFun
              ( [ PVar "rules" ],
                List.fold_left
                  (fun rules rule ->
                    List.fold_left
                      (fun rules rule -> EApp (cons_fun, [ rule; rules ]))
                      rules (rev_branch_expr rule))
                  (EVar "rules")
                  (StringMap.fold
                     (fun _ rule rev_rules -> rule :: rev_rules)
                     G.grammar.exported_rules []) ),
            type2scheme extend_typ );
    }

  let interface =
    [
      IIFunctor
        ( "Make",
          G.grammar.parameters,
          [
            IIModuleType ("SYMBOLS", MTSigEnd [ IIValDecls exportvaldefs ]);
            (if exportdefs = [] then IIComment "No symbol definition"
             else
               IIModule
                 ( "Symbols",
                   if List.length exportvaldefs = List.length exportdefs then
                     MTNamedModuleType "SYMBOLS"
                   else
                     MTSigEnd
                       [
                         IIValDecls
                           (List.map
                              (fun { valpat; valval; _ } ->
                                match (valpat, valval) with
                                | PVar symbol, EAnnot (_, scheme) ->
                                    (symbol, scheme)
                                | _ -> assert false)
                              exportdefs);
                       ] ));
            IIFunctor
              ( "Grammar",
                [ Located.locate Range.dummy "_ : SYMBOLS" ],
                [
                  IIValDecls
                    [
                      (*                       ("entries", type2scheme start_typ); *)
                      ("extend", type2scheme extend_typ);
                    ];
                ] );
          ] );
    ]

  let program =
    [
      SIFunctor
        ( "Make",
          G.grammar.parameters,
          [
            SIComment
              (* Make a reference to [MenhirLib.StaticVersion.require_XXXXXXXX], where
       [XXXXXXXX] is our 8-digit version number. This ensures that the
       generated code can be linked only with an appropriate version of
       MenhirLib. This is important because we use unsafe casts, and a
       version mismatch could cause a crash. *)
              "This generated code requires the following version of MenhirLib:";
            valdef versiondef;
            SIFragment G.grammar.preludes;
            SIModuleTypeDef ("SYMBOLS", MTSigEnd [ IIValDecls exportvaldefs ]);
            SIValDefs (false, filedefs);
            SIValDefs (false, precedencedefs);
            SIModuleDef ("Symbols", MStruct [ SIValDefs (false, exportdefs) ]);
            SIFunctor
              ( "Grammar",
                [ Located.locate Range.dummy "Symbols : SYMBOLS" ],
                SIFragment [ Located.locate Range.dummy "open Symbols" ]
                :: [
                     SIValDefs (false, localdefs);
                     SIValDefs (false, [ (* startdef; *) extenddef ]);
                   ] );
            SIFragment G.grammar.postludes;
          ] );
    ]

  let () = Time.stop start_time "Producing partial grammar functor"
end
(* Run *)
