open Tannot
open Asttypes
(*let todo = failwith "UwU"*)


let apply_env = ref []

(*some functions shouldn't be defunctionalised and thus shouldn't be counted as free vars or when creating the func type*)
let legit_funcs = ["print_string";"print_int";"print_newline"]

let is_legit x : bool = List.mem x legit_funcs

let identifier,reset_id = 
  let funid = ref (-1) in
  ((fun () -> incr funid;!funid),fun () -> funid := -1)

let rec print_ttyp : Typedtree.typ -> unit = function
  | Tvar {def = Some t} -> print_ttyp t
  | Tvar v -> print_string "'a";print_int v.id
  | Tarrow (a,b) -> print_ttyp a; print_string " -> ";print_ttyp b
  | Tint -> print_string "int"
  | Tunit -> print_string "unit"
  | Tbool -> print_string "bool"
  | Tstring -> print_string "string"
  | Tlist t -> print_ttyp t; print_string " list"
  | Ttuple [x] -> print_ttyp x
  | Ttuple (h::t) -> print_ttyp h; print_string " * ";print_ttyp (Ttuple t)
  | Ttuple [] -> failwith "wtf"

let rec type_translate (_t : Typedtree.typ) : Tannot.typ = match _t with
  | Tvar {def = Some t} -> type_translate t
  | Tvar {id;level}-> Tvar {id = id; level = level}
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

let rec bound_vars_tpatt (pat : Typedtree.tpatt) : (ident * typ) list = match pat.tpatt_desc with
  | TP_any -> []
  | TP_ident x -> [x,type_translate pat.tpatt_typ]
  | TP_tuple ls -> List.fold_left (fun ls x -> (bound_vars_tpatt x) @ ls) [] ls

(*checks if a var is already part of the bound vars*)
let rec isin x = function
  | [] -> false
  | (h,_)::t -> x = h || isin x t

let rec union l1 l2 = match l1 with
  |((a,_) as h)::t -> if isin a l2 then union t l2 else union t (h::l2)
  |[] -> l2 

let rec union2 = function
  |[] -> []
  |[l] -> l
  |h::t -> union h (union2 t)

let add_tpatt ls pat = union (bound_vars_tpatt pat) ls 
let (++) pat ls = add_tpatt ls pat

let rec print_typ : typ -> unit = function
  | Tvar v -> print_string "'a";print_int v.id
  | Tarrow (a,b) -> print_typ a; print_string " -> ";print_typ b
  | Tint -> print_string "int"
  | Tunit -> print_string "unit"
  | Tbool -> print_string "bool"
  | Tstring -> print_string "string"
  | Tlist t -> print_typ t; print_string " list"
  | Ttuple [x] -> print_typ x
  | Ttuple (h::t) -> print_typ h; print_string " * ";print_typ (Ttuple t)
  | Ttuple [] -> failwith "wtf"

let print_list pp l = 
  let rec aux = function
    | [] -> print_endline "]"
    | [x] -> pp x; print_endline "]"
    | h::t -> pp h;print_char ',';aux t
  in print_char '['; aux l

let print_tup pp1 pp2 x =
  print_char '(';pp1 (fst x);print_char ',';pp2 (snd x);print_char ')'

let print_free_vars l = print_list (print_tup print_string print_typ) l

(*finds free vars recursively*)
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
  | TE_ident x -> if isin x bound_vars || is_legit x then [] else [(x,type_translate e.texpr_typ)]
  | TE_match (em, e1, (pat1, pat2, e2)) -> union2 [free_vars bound_vars em;
                                                   free_vars bound_vars e1;
                                                   free_vars (pat1 ++ (pat2 ++ bound_vars)) e2] 


(*Typedtree.texpr to Tannot.texpr*)                                                   
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
  | TE_fun (pat,e) -> 
    let fv =  free_vars [] _ds in 
    (*print_free_vars fv;*)
    TE_fun(tpatt_translate pat, texpr_translate e, identifier (), fv)
  | TE_ident x -> TE_ident x
  | TE_match (e1, e2, (pat1, pat2, e3)) -> TE_match (texpr_translate e1, texpr_translate e2,
                                                      (tpatt_translate pat1, tpatt_translate pat2, texpr_translate e3))

in {texpr_desc = expr_desc; 
    texpr_typ = type_translate _ds.texpr_typ}

let tdef_translate ( (is_rec, pat, e) : Typedtree.t_def) : t_def =
    (is_rec, tpatt_translate pat, texpr_translate e)

let file (_ds: Typedtree.t_def list): t_def list =
  let res =
    List.map tdef_translate _ds in
  reset_id (); (*so that each test always starts with F0*)
  res
