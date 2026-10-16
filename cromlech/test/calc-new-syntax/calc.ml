
module type SYMBOLS = sig
  
  val tk_times: (unit) Cromlech.Grammar.terminal
  
  val tk_rparen: (unit) Cromlech.Grammar.terminal
  
  val tk_plus: (unit) Cromlech.Grammar.terminal
  
  val tk_minus: (unit) Cromlech.Grammar.terminal
  
  val tk_lparen: (unit) Cromlech.Grammar.terminal
  
  val tk_int: 
# 1 "demos/calc-new-syntax/parser.mly"
       (int)
# 18 "demos/calc-new-syntax/parser2.ml"
   Cromlech.Grammar.terminal
  
  val tk_eol: (unit) Cromlech.Grammar.terminal
  
  val tk_div: (unit) Cromlech.Grammar.terminal
  
  val nt_main: (int) Cromlech.Grammar.nonterminal
  
end

let _standard_mly_  = "<standard.mly>"

and demos_calc_new_syntax_parser_mly = "demos/calc-new-syntax/parser.mly"



module Symbols = struct
  
  let tk_times : (unit) Cromlech.Grammar.terminal =
    let tk_times__position =  ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 35;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 40;
    }) in
    Cromlech.Grammar.terminal ~position:tk_times__position "TIMES" UndefinedAssoc UndefinedPrecedence
  
  and tk_rparen : (unit) Cromlech.Grammar.terminal =
    let tk_rparen__position =  ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 3;
      pos_bol = 45;
      pos_cnum = 59;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 3;
      pos_bol = 45;
      pos_cnum = 65;
    }) in
    Cromlech.Grammar.terminal ~position:tk_rparen__position "RPAREN" UndefinedAssoc UndefinedPrecedence
  
  and tk_plus : (unit) Cromlech.Grammar.terminal =
    let tk_plus__position =  ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 24;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 28;
    }) in
    Cromlech.Grammar.terminal ~position:tk_plus__position "PLUS" UndefinedAssoc UndefinedPrecedence
  
  and tk_minus : (unit) Cromlech.Grammar.terminal =
    let tk_minus__position = ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 29;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 34;
    }) in
    Cromlech.Grammar.terminal ~position:tk_minus__position "MINUS" UndefinedAssoc UndefinedPrecedence
  
  and tk_lparen : (unit) Cromlech.Grammar.terminal =
    let tk_lparen__position = ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 3;
      pos_bol = 45;
      pos_cnum = 52;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 3;
      pos_bol = 45;
      pos_cnum = 58;
    }) in
    Cromlech.Grammar.terminal ~position:tk_lparen__position "LPAREN" UndefinedAssoc UndefinedPrecedence
  
  and tk_int : 
# 1 "demos/calc-new-syntax/parser.mly"
       (int)
# 114 "demos/calc-new-syntax/parser2.ml"
   Cromlech.Grammar.terminal =
    let tk_int__position = ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 13;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 16;
    }) in
    Cromlech.Grammar.terminal ~position:tk_int__position "INT" UndefinedAssoc UndefinedPrecedence
  
  and tk_eol : (unit) Cromlech.Grammar.terminal =
    let tk_eol__position = ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 4;
      pos_bol = 66;
      pos_cnum = 73;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 4;
      pos_bol = 66;
      pos_cnum = 76;
    }) in
    Cromlech.Grammar.terminal ~position:tk_eol__position "EOL" UndefinedAssoc UndefinedPrecedence
  
  and tk_div : (unit) Cromlech.Grammar.terminal =
    let tk_div__position = ({
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 41;
    }, {
      Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
      pos_lnum = 2;
      pos_bol = 17;
      pos_cnum = 44;
    }) in
    Cromlech.Grammar.terminal ~position:tk_div__position "DIV" UndefinedAssoc UndefinedPrecedence
  
  and nt_main : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:false "main"
  
end

module Grammar
(Symbols : SYMBOLS)
= struct
  open Symbols
  let nt_unary_op : (int -> int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "unary_op"
  
  and nt_multiplicative_op : (int -> int -> int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "multiplicative_op"
  
  and nt_multiplicative_expr : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "multiplicative_expr"
  
  and nt_fold_left_multiplicative_op_atomic_expr_ : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:false "fold_left(multiplicative_op,atomic_expr)"
  
  and nt_fold_left_additive_op_multiplicative_expr_ : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:false "fold_left(additive_op,multiplicative_expr)"
  
  and nt_expr : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "expr"
  
  and nt_delimited_lparen_expr_rparen_ : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "delimited(LPAREN,expr,RPAREN)"
  
  and nt_atomic_expr : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:false "atomic_expr"
  
  and nt_app_unary_op_atomic_expr_ : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "app(unary_op,atomic_expr)"
  
  and nt_additive_op : (int -> int -> int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "additive_op"
  
  and nt_additive_expr : (int) Cromlech.Grammar.nonterminal =
    Cromlech.Grammar.nonterminal ~inline:true "additive_expr"
  
  let extend : (Cromlech.Grammar.rule) list -> (Cromlech.Grammar.rule) list =
    fun rules ->
      List.cons (let nt_additive_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 53;
        pos_bol = 1667;
        pos_cnum = 1669;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 53;
        pos_bol = 1667;
        pos_cnum = 1712;
      }) in
      let nt_additive_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 37) in
      let nt_additive_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_additive_expr__source ~level:nt_additive_expr__level ?precedence:nt_additive_expr__precedence nt_additive_expr (Bind (NonTerminal nt_fold_left_additive_op_multiplicative_expr_, Return)) (fun _1 ->
        _1)) (List.cons (let nt_additive_op__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 58;
        pos_bol = 1792;
        pos_cnum = 1796;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 58;
        pos_bol = 1792;
        pos_cnum = 1812;
      }) in
      let nt_additive_op__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 38) in
      let nt_additive_op__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_additive_op__source ~level:nt_additive_op__level ?precedence:nt_additive_op__precedence nt_additive_op (Bind (Terminal tk_plus, Return)) (fun _1 ->
        
# 58 "demos/calc-new-syntax/parser.mly"
           ( ( + ) )
# 232 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_additive_op__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 59;
        pos_bol = 1813;
        pos_cnum = 1817;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 59;
        pos_bol = 1813;
        pos_cnum = 1833;
      }) in
      let nt_additive_op__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 39) in
      let nt_additive_op__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_additive_op__source ~level:nt_additive_op__level ?precedence:nt_additive_op__precedence nt_additive_op (Bind (Terminal tk_minus, Return)) (fun _1 ->
        
# 59 "demos/calc-new-syntax/parser.mly"
           ( ( - ) )
# 250 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_app_unary_op_atomic_expr___source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 31;
        pos_bol = 1012;
        pos_cnum = 1014;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 31;
        pos_bol = 1012;
        pos_cnum = 1035;
      }) in
      let nt_app_unary_op_atomic_expr___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 32) in
      let nt_app_unary_op_atomic_expr___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_app_unary_op_atomic_expr___source ~level:nt_app_unary_op_atomic_expr___level ?precedence:nt_app_unary_op_atomic_expr___precedence nt_app_unary_op_atomic_expr_ (Bind (NonTerminal nt_unary_op, Bind (NonTerminal nt_atomic_expr, Return))) (fun f x ->
        
# 31 "demos/calc-new-syntax/parser.mly"
                ( f x )
# 268 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_atomic_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 79;
        pos_bol = 2351;
        pos_cnum = 2355;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 79;
        pos_bol = 2351;
        pos_cnum = 2358;
      }) in
      let nt_atomic_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 45) in
      let nt_atomic_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_atomic_expr__source ~level:nt_atomic_expr__level ?precedence:nt_atomic_expr__precedence nt_atomic_expr (Bind (Terminal tk_int, Return)) (fun _1 ->
        _1)) (List.cons (let nt_atomic_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 80;
        pos_bol = 2359;
        pos_cnum = 2363;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 80;
        pos_bol = 2359;
        pos_cnum = 2394;
      }) in
      let nt_atomic_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 49) in
      let nt_atomic_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_atomic_expr__source ~level:nt_atomic_expr__level ?precedence:nt_atomic_expr__precedence nt_atomic_expr (Bind (NonTerminal nt_delimited_lparen_expr_rparen_, Return)) (fun _1 ->
        _1)) (List.cons (let nt_atomic_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 81;
        pos_bol = 2395;
        pos_cnum = 2399;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 81;
        pos_bol = 2395;
        pos_cnum = 2425;
      }) in
      let nt_atomic_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 52) in
      let nt_atomic_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_atomic_expr__source ~level:nt_atomic_expr__level ?precedence:nt_atomic_expr__precedence nt_atomic_expr (Bind (NonTerminal nt_app_unary_op_atomic_expr_, Return)) (fun _1 ->
        _1)) (List.cons (let nt_delimited_lparen_expr_rparen___source = ({
        Lexing.pos_fname = "<standard.mly>";
        pos_lnum = 204;
        pos_bol = 5423;
        pos_cnum = 5425;
      }, {
        Lexing.pos_fname = "<standard.mly>";
        pos_lnum = 204;
        pos_bol = 5423;
        pos_cnum = 5448;
      }) in
      let nt_delimited_lparen_expr_rparen___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (_standard_mly_, 17) in
      let nt_delimited_lparen_expr_rparen___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_delimited_lparen_expr_rparen___source ~level:nt_delimited_lparen_expr_rparen___level ?precedence:nt_delimited_lparen_expr_rparen___precedence nt_delimited_lparen_expr_rparen_ (Ignore (Terminal tk_lparen, Bind (NonTerminal nt_expr, Ignore (Terminal tk_rparen, Return)))) (fun x ->
        
# 205 "<standard.mly>"
    ( x )
# 328 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 47;
        pos_bol = 1503;
        pos_cnum = 1505;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 47;
        pos_bol = 1503;
        pos_cnum = 1518;
      }) in
      let nt_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 34) in
      let nt_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_expr__source ~level:nt_expr__level ?precedence:nt_expr__precedence nt_expr (Bind (NonTerminal nt_additive_expr, Return)) (fun _1 ->
        _1)) (List.cons (let nt_fold_left_additive_op_multiplicative_expr___source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 22;
        pos_bol = 687;
        pos_cnum = 691;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 22;
        pos_bol = 687;
        pos_cnum = 695;
      }) in
      let nt_fold_left_additive_op_multiplicative_expr___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 28) in
      let nt_fold_left_additive_op_multiplicative_expr___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_fold_left_additive_op_multiplicative_expr___source ~level:nt_fold_left_additive_op_multiplicative_expr___level ?precedence:nt_fold_left_additive_op_multiplicative_expr___precedence nt_fold_left_additive_op_multiplicative_expr_ (Bind (NonTerminal nt_multiplicative_expr, Return)) (fun _1 ->
        _1)) (List.cons (let nt_fold_left_additive_op_multiplicative_expr___source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 23;
        pos_bol = 696;
        pos_cnum = 700;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 23;
        pos_bol = 696;
        pos_cnum = 760;
      }) in
      let nt_fold_left_additive_op_multiplicative_expr___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 31) in
      let nt_fold_left_additive_op_multiplicative_expr___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_fold_left_additive_op_multiplicative_expr___source ~level:nt_fold_left_additive_op_multiplicative_expr___level ?precedence:nt_fold_left_additive_op_multiplicative_expr___precedence nt_fold_left_additive_op_multiplicative_expr_ (Bind (NonTerminal nt_fold_left_additive_op_multiplicative_expr_, Bind (NonTerminal nt_additive_op, Bind (NonTerminal nt_multiplicative_expr, Return)))) (fun sum op elem ->
        
# 23 "demos/calc-new-syntax/parser.mly"
                                                 ( op sum elem )
# 374 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_fold_left_multiplicative_op_atomic_expr___source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 22;
        pos_bol = 687;
        pos_cnum = 691;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 22;
        pos_bol = 687;
        pos_cnum = 695;
      }) in
      let nt_fold_left_multiplicative_op_atomic_expr___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 28) in
      let nt_fold_left_multiplicative_op_atomic_expr___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_fold_left_multiplicative_op_atomic_expr___source ~level:nt_fold_left_multiplicative_op_atomic_expr___level ?precedence:nt_fold_left_multiplicative_op_atomic_expr___precedence nt_fold_left_multiplicative_op_atomic_expr_ (Bind (NonTerminal nt_atomic_expr, Return)) (fun _1 ->
        _1)) (List.cons (let nt_fold_left_multiplicative_op_atomic_expr___source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 23;
        pos_bol = 696;
        pos_cnum = 700;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 23;
        pos_bol = 696;
        pos_cnum = 760;
      }) in
      let nt_fold_left_multiplicative_op_atomic_expr___level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 31) in
      let nt_fold_left_multiplicative_op_atomic_expr___precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_fold_left_multiplicative_op_atomic_expr___source ~level:nt_fold_left_multiplicative_op_atomic_expr___level ?precedence:nt_fold_left_multiplicative_op_atomic_expr___precedence nt_fold_left_multiplicative_op_atomic_expr_ (Bind (NonTerminal nt_fold_left_multiplicative_op_atomic_expr_, Bind (NonTerminal nt_multiplicative_op, Bind (NonTerminal nt_atomic_expr, Return)))) (fun sum op elem ->
        
# 23 "demos/calc-new-syntax/parser.mly"
                                                 ( op sum elem )
# 406 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_main__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 42;
        pos_bol = 1422;
        pos_cnum = 1424;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 42;
        pos_bol = 1422;
        pos_cnum = 1441;
      }) in
      let nt_main__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 33) in
      let nt_main__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_main__source ~level:nt_main__level ?precedence:nt_main__precedence nt_main (Bind (NonTerminal nt_expr, Bind (Terminal tk_eol, Return))) (fun expr _2 ->
        expr)) (List.cons (let nt_multiplicative_expr__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 65;
        pos_bol = 1991;
        pos_cnum = 1993;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 65;
        pos_bol = 1991;
        pos_cnum = 2034;
      }) in
      let nt_multiplicative_expr__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 42) in
      let nt_multiplicative_expr__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_multiplicative_expr__source ~level:nt_multiplicative_expr__level ?precedence:nt_multiplicative_expr__precedence nt_multiplicative_expr (Bind (NonTerminal nt_fold_left_multiplicative_op_atomic_expr_, Return)) (fun _1 ->
        _1)) (List.cons (let nt_multiplicative_op__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 70;
        pos_bol = 2126;
        pos_cnum = 2130;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 70;
        pos_bol = 2126;
        pos_cnum = 2146;
      }) in
      let nt_multiplicative_op__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 43) in
      let nt_multiplicative_op__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_multiplicative_op__source ~level:nt_multiplicative_op__level ?precedence:nt_multiplicative_op__precedence nt_multiplicative_op (Bind (Terminal tk_times, Return)) (fun _1 ->
        
# 70 "demos/calc-new-syntax/parser.mly"
           ( ( * ) )
# 452 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_multiplicative_op__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 71;
        pos_bol = 2147;
        pos_cnum = 2151;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 71;
        pos_bol = 2147;
        pos_cnum = 2167;
      }) in
      let nt_multiplicative_op__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 44) in
      let nt_multiplicative_op__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_multiplicative_op__source ~level:nt_multiplicative_op__level ?precedence:nt_multiplicative_op__precedence nt_multiplicative_op (Bind (Terminal tk_div, Return)) (fun _1 ->
        
# 71 "demos/calc-new-syntax/parser.mly"
           ( ( / ) )
# 470 "demos/calc-new-syntax/parser2.ml"
        )) (List.cons (let nt_unary_op__source = ({
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 86;
        pos_bol = 2499;
        pos_cnum = 2503;
      }, {
        Lexing.pos_fname = "demos/calc-new-syntax/parser.mly";
        pos_lnum = 86;
        pos_bol = 2499;
        pos_cnum = 2519;
      }) in
      let nt_unary_op__level : (MenhirMrk.MiddleAPI.production_level) = ProductionLevel (demos_calc_new_syntax_parser_mly, 53) in
      let nt_unary_op__precedence : ((string) MenhirMrk.MiddleAPI.located * (MenhirMrk.MiddleAPI.precedence_level)) option = None in
      Cromlech.Grammar.rule ~source:nt_unary_op__source ~level:nt_unary_op__level ?precedence:nt_unary_op__precedence nt_unary_op (Bind (Terminal tk_minus, Return)) (fun _1 ->
        
# 86 "demos/calc-new-syntax/parser.mly"
           ( (~- ) )
# 488 "demos/calc-new-syntax/parser2.ml"
        )) rules)))))))))))))))))
  
end
