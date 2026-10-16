(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module expands away the keywords [$startpos] and [$endpos] as well as
   the entire [ofs] family of keywords. Doing this early simplifies some aspects
   later on, in particular inlining. *)

open PlainSyntax

module Lib (Ctx : sig
  type producer
  type producers = producer list

  val prod_id : producer -> string
  val prod_symbol : producer -> string

  val nullable : symbol -> bool
  (**[nullable symbol] returns [true] if the symbol may be nullable. *)

  val epsilon : symbol -> bool
  (**[epsilon symbol] returns [true] if the symbol generates (a subset of) the language
   {epsilon}. *)
end) : sig
  val expand_action : Ctx.producers -> action -> action
  (**[expand_action] expands away the keywords [$startpos] and [$endpos] in
     semantic actions. The entire [ofs] family of keywords is also expanded
     away. *)
end

val expand : grammar -> grammar
(**[expand] expands away the keywords [$startpos] and [$endpos] in semantic
   actions. The entire [ofs] family of keywords is also expanded away. This
   transformation does NOT affect %merge functions, where we allow the keywords
   [$startpos] and [$endpos] and do not allow any other keyword. *)
