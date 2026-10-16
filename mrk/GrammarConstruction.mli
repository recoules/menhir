(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs a rich view of the grammar,
   which obeys the signature [GRAMMAR]. *)

open MiddleAPI

module type G = sig
   type ocamltype
   type merge_fun
   type attributes
   type action
   module Nonterminal : NONTERMINAL with type t = int
                            and type ocamltype := ocamltype
                            and type merge_fun := merge_fun
                            and type attributes := attributes
   module Terminal : TERMINAL with type t = int
                      and type ocamltype := ocamltype 
                      and type attributes := attributes
   module Production : PRODUCTION with type t = int
                          and type terminal := Terminal.t
                          and type nonterminal := Nonterminal.t
                          and type attributes := attributes
                          and type action := action
   val attributes : attributes
   val on_error_reduce : production_level StringMap.t
end

module Make (G : G) ()
: GRAMMAR with type ocamltype = G.ocamltype
           and type merge_fun = G.merge_fun
           and type attributes = G.attributes
           and type action = G.action
