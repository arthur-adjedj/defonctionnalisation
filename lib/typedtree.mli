(* Typed AST *)
open Asttypes

(** A "type variable"
    This is either:
    - an actual type variable (some 'a) if [def] is [None].
      in that case, [id] is a unique identifier for the
      variable. [level] is used to indicate the place
      at which the variable is bound (but we do not need
      to worry about that here).

    - an alias for type [ty] if [def] is [Some ty].
      (this corresponds to a type variable that got instatiated)
*)
type tvar =
  { id : int;
    mutable level : int;
    mutable def : typ option }

(* types *)
and typ =
  | Tvar of tvar
  | Tarrow of typ * typ (** τ1 -> τ2 *)
  | Tint (** int *)
  | Tunit (** unit *)
  | Tbool (** bool *)
  | Tstring (** string *)
  | Tlist of typ (** τ list *)
  | Ttuple of typ list (** τ1 * ... * τn *)

(* an expression annotated with its type *)
type texpr =
  { texpr_desc : texpr_desc;
    texpr_typ  : typ; }

(* expressions *)
and texpr_desc =
  | TE_cte of constant (** see Asttypes.constant *)
  | TE_unop of unop * texpr (** not e, - e *)
  | TE_binop of binop * texpr * texpr (** e1 + e2, e1 = e2, ... (see Asttypes.binop) *)
  | TE_if of texpr * texpr * texpr (** if e1 then e2 else e3 *)
  | TE_nil (** [] *)
  | TE_cons of texpr * texpr (** e1 :: e2 *)
  | TE_app of texpr * texpr (** e1 e2 *)
  | TE_tuple of texpr list (** (e1, ..., en) *)
  | TE_let of is_rec * tpatt * texpr * texpr
    (** let p = e1 in e2     (if is_rec flag is false)
        let rec p = e1 in e2 (if is_rec flag is true)
    *)
  | TE_fun of tpatt * texpr (** function p -> e *)
  | TE_ident of ident (** x *)
  | TE_match of texpr * texpr * (tpatt * tpatt * texpr)
    (** match e1 with
        | [] -> e2
        | p1 :: p2 -> e3
    *)

(* patterns annotated with their type *)
and tpatt =
  { tpatt_desc : tpatt_desc;
    tpatt_typ  : typ }

(** patterns *)
and tpatt_desc =
  | TP_any (** _ *)
  | TP_ident of ident (** x *)
  | TP_tuple of tpatt list (** (p1, .., pn) *)

(** toplevel declaration
    let p = e     (if is_rec flag is false)
    let rec p = e (if is_rec flag is true)
*)
type t_def = is_rec * tpatt * texpr
