(** Target AST: abstract syntax tree for the defunctionalized programs.

    A defonctionalized program consists of:
    - a GADT declaration (named [func])
    - a recursive [apply] function, matching on values of the [func] GADT;
      the implementation of [apply] itself does not uses functions
    - a list of toplevel declarations, that do not use functions
*)
open Asttypes

(** type variable names ('a, 'b, ...) (without the "'") *)
type tyident = string

(** GADT constructor name (e.g. Foo, Bar) *)
type cident = string

(** identifier appearing in a pattern *)
type pident = string

(** program identifier. This is either:
    - an identifier that appeared in the source program (case [ID_string])
    - the name of the [apply] function ([ID_apply])
    - the name of the second argument of the [apply] function ([ID_arg])
*)
type ident =
  | ID_apply
  | ID_arg
  | ID_string of string

(** patterns *)
type tgt_patt =
  | TP_any
  | TP_ident of pident
  | TP_tuple of tgt_patt list

(** expressions (nb: no functions) *)
type tgt_expr =
  | TE_cte of constant
  | TE_unop of unop * tgt_expr
  | TE_binop of binop * tgt_expr * tgt_expr
  | TE_if of tgt_expr * tgt_expr * tgt_expr
  | TE_nil
  | TE_cons of tgt_expr * tgt_expr
  | TE_app of tgt_expr * tgt_expr
  | TE_tuple of tgt_expr list
  | TE_let of is_rec * tgt_patt * tgt_expr * tgt_expr
  | TE_ident of ident
  | TE_match of tgt_expr * tgt_expr * (tgt_patt * tgt_patt * tgt_expr)
  | TE_func_constr of cident * string list
    (** Constructor of [func]: [Fn (x1, ..., xn)] *)

(** a toplevel declaration
    [let p = e]  or
    [let rec p = e]  (depending on the [is_rec] flag)
 *)
type tgt_def = is_rec * tgt_patt * tgt_expr

(** types *)
type ctyp =
  | TTY_var of tyident (** 'a, 'b *)
  | TTY_func of ctyp * ctyp (** (τ1, τ2) func *)
  | TTY_int (** int *)
  | TTY_unit (** unit *)
  | TTY_bool (** bool *)
  | TTY_string (** string *)
  | TTY_list of ctyp (** τ list *)
  | TTY_tuple of ctyp list (** τ1 * ... * τn *)

(** Type of a constructor declaration for the [func] GADT, of the form:

    | Cname : τ1 * .. * τn -> (τarg, τret) func
*)
type tgt_func_constr_ty =
  { constr_ty_params : ctyp list; (** τ1 .. τn *)
    constr_ty_arg : ctyp; (** τarg *)
    constr_ty_ret : ctyp (** τret *) }

(** A complete program, of the form:

    type (_, _) func =
    | C1 : ... -> (.., ..) func
    ...

    let rec apply : type a r. (a, r) func -> a -> r = fun f arg ->
      match f with
      | C1 (x1, .. xn) -> ...
      ...
*)
type tgt_file =
  { tfile_func : (cident * tgt_func_constr_ty) list;
    (** constructor declarations for [func] *)

    tfile_apply : (cident * string list * tgt_expr) list;
    (** Implementation of [apply], for each constructor of [func]:
        - name of the constructor
        - list of variables for the constructor parameters
        - expression for the corresponding branch of the match
    *)

    tfile_defs : tgt_def list
    (** Toplevel declarations *)
  }
