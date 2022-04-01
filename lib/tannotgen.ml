open Tannot
open Asttypes
let todo = failwith "UwU"

let identifier = 
  let funid = ref (-1) in
  function () -> incr funid;!funid


let rec type_translate (_t : Typedtree.typ) : Tannot.typ = match _t with
  | Tvar {id;level}-> Tvar {id = id; level = level} (*100% doesn't work*)
  | Tarrow (t1,t2) -> Tarrow (type_translate t1,type_translate t2)
  | Tint -> Tint
  | Tunit -> Tunit
  | Tbool -> Tbool
  | Tstring -> Tstring
  | Tlist t -> Tlist (type_translate t)
  | Ttuple ls -> Ttuple (List.map type_translate ls)

let rec tpatt_translate (_tp : Typedtree.tpatt) : tpatt = 
  let desc = match _tp.tpatt_desc with
    | TP_any -> TP_any
    | TP_ident x -> TP_ident x
    | TP_tuple ls -> TP_tuple (List.map tpatt_translate ls)
  in {tpatt_desc =  desc;
      tpatt_typ = type_translate _tp.tpatt_typ}

let rec free_vars_tpatt (pat : Typedtree.tpatt) : (ident * typ) list = match pat.tpatt_desc with
  | TP_any -> []
  | TP_ident x -> [x,type_translate pat.tpatt_typ]
  | TP_tuple ls -> List.fold_left (fun ls x -> (free_vars_tpatt x) @ ls) [] ls

let rec union l1 l2 = match l1 with
  |h::t -> if List.mem h l2 then union t l2 else union t (h::l2)
  |[] -> l2 

let rec union2 = function
  |[] -> []
  |[l] -> l
  |h::t -> union h (union2 t)

let add_tpatt ls pat = union (free_vars_tpatt pat) ls 
let (++) pat ls = add_tpatt ls pat

(*TODO refactor*)
let rec free_vars (bound_vars : (ident * typ) list) (e : Typedtree.texpr)  : (string * typ) list = match e.texpr_desc with
  | TE_cte _ -> []
  | TE_unop (_,e) -> free_vars bound_vars e
  | TE_binop (_,e1,e2) -> union (free_vars bound_vars e1) (free_vars bound_vars e2)
  | TE_if (eb,e1,e2) -> union2 (List.map (free_vars bound_vars) [eb;e1;e2])
  | TE_nil  -> []
  | TE_cons (e1,e2) -> union (free_vars bound_vars e1) (free_vars bound_vars e2)
  | TE_app (e1,e2) -> union (free_vars bound_vars e1) (free_vars bound_vars e2)
  | TE_tuple ls -> union2 (List.map (free_vars bound_vars) ls)
  | TE_let (true, pat, e1, e2) -> union (free_vars (pat ++ bound_vars) e1) (free_vars (pat ++ bound_vars) e2)
  | TE_let (false, pat, e1, e2) -> union (free_vars bound_vars e1) (free_vars (pat ++ bound_vars) e2)
  | TE_fun (pat,e) -> free_vars (pat ++ bound_vars) e
  | TE_ident x -> if List.mem (x,type_translate e.texpr_typ) bound_vars then [] else [(x,type_translate e.texpr_typ)]
  | TE_match (em, e1, (pat1, pat2, e2)) -> union2 [free_vars bound_vars em;
                                                   free_vars bound_vars e1;
                                                   free_vars (pat1 ++ (pat2 ++ bound_vars)) e2] 

let rec texpr_translate (_ds : Typedtree.texpr) : Tannot.texpr = 
  let expr_desc = match _ds.texpr_desc with
  | TE_cte c -> TE_cte c
  | TE_unop (op,e) -> TE_unop (op,texpr_translate e)
  | TE_binop (op,e1,e2) -> TE_binop (op,texpr_translate e1,texpr_translate e2)
  | TE_if (eb,e1,e2) -> TE_if (texpr_translate eb, texpr_translate e1, texpr_translate e2)
  | TE_nil  -> TE_nil
  | TE_cons (e1,e2) -> TE_cons (texpr_translate e1, texpr_translate e2)
  | TE_app (e1,e2) -> TE_app (texpr_translate e1, texpr_translate e2)
  | TE_tuple ls -> TE_tuple (List.map texpr_translate ls)
  | TE_let (is_rec, pat, e1, e2) -> TE_let (is_rec, tpatt_translate pat,texpr_translate e1,texpr_translate e2)
  | TE_fun (pat,e) -> TE_fun(tpatt_translate pat, texpr_translate e, identifier (), free_vars (free_vars_tpatt pat) e)
  | TE_ident x -> TE_ident x
  | TE_match (e1, e2, (pat1, pat2, e3)) -> TE_match (texpr_translate e1, texpr_translate e2,
                                                      (tpatt_translate pat1, tpatt_translate pat2, texpr_translate e3))

in {texpr_desc = expr_desc; 
    texpr_typ = type_translate _ds.texpr_typ}

let tdef_translate ( (is_rec, pat, e) : Typedtree.t_def) : t_def =
    (is_rec, tpatt_translate pat, texpr_translate e)

let file (_ds: Typedtree.t_def list): t_def list =
  List.map tdef_translate _ds
