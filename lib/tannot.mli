(** "Annotated" typed tree. This is similar to the typedtree, but with the
   following differences:

    - functions are annotated with a unique identifier (different for each
    lambda-abstraction), and the list of free variables (and their types) that
    appear in the function.

    - indirections through [tvar.def] have been expanded, so that [tvar] always
   denotes an actual type variable.
 *)

open Asttypes

type funid = int

type tvar =
  { id : int;
    level : int }

and typ =
  | Tvar of tvar
  | Tarrow of typ * typ
  | Tint
  | Tunit
  | Tbool
  | Tstring
  | Tlist of typ
  | Ttuple of typ list

type fclos = (ident * typ) list

type texpr =
  { texpr_desc : texpr_desc;
    texpr_typ  : typ }

and texpr_desc =
  | TE_cte of constant
  | TE_unop of unop * texpr
  | TE_binop of binop * texpr * texpr
  | TE_if of texpr * texpr * texpr
  | TE_nil
  | TE_cons of texpr * texpr
  | TE_app of texpr * texpr
  | TE_tuple of texpr list
  | TE_let of is_rec * tpatt * texpr * texpr
  | TE_fun of tpatt * texpr * funid * fclos
  (** Function case:
      - the argument of type [funid] (=int) is the function's unique identifier;
      - the argument of type [fclos] is the list of free variables of the function
        (ie. free variables of the body minus the variables bound by the function)
  *)
  | TE_ident of ident
  | TE_match of texpr * texpr * (tpatt * tpatt * texpr)

and tpatt =
  { tpatt_desc : tpatt_desc;
    tpatt_typ  : typ }

and tpatt_desc =
  | TP_any
  | TP_ident of ident
  | TP_tuple of tpatt list

type t_def = is_rec * tpatt * texpr
