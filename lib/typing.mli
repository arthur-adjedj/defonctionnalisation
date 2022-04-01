(* Printing *)

val print_type : Format.formatter -> Typedtree.typ -> unit

(* Typing *)

val file : Parsetree.p_def list -> Typedtree.t_def list

(* Errors *)

type error =
  | UnificationFailure of Typedtree.typ * Typedtree.typ
  | RecursiveValue
  | UnboundVariable of Asttypes.ident
  | DuplicatePatIdent of Asttypes.ident

exception Error of Asttypes.location * error
val report : Format.formatter -> error -> unit
