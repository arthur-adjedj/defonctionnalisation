open Tannot
open Target

(*let todo = failwith "UWU"*)

let arg = TE_ident ID_arg
let apply = TE_ident ID_apply


let func_env  : (cident * tgt_func_constr_ty) list ref = 
  ref []

let apply_env : (cident * cident list * tgt_expr) list ref = ref []

let legit_funcs = ["print_string";"print_int";"print_newline"]

(* a function is called legit if it shouldn't be defunctionalised*)
let is_legit = function
  | TE_ident (ID_string id) -> List.mem id legit_funcs
  | _ -> false


let rec tpatt_translate (pat : Tannot.tpatt)  : Target.tgt_patt = match pat.tpatt_desc with
  | TP_any -> TP_any
  | TP_ident x -> TP_ident x
  | TP_tuple ls -> TP_tuple (List.map tpatt_translate ls) 

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

let rec type_translate = function 
  | Tvar v -> TTY_var ("'a"^string_of_int v.id)
  | Tarrow (a, b)-> TTY_func(type_translate a,type_translate b)
  | Tint -> TTY_int
  | Tunit -> TTY_unit
  | Tbool -> TTY_bool
  | Tstring -> TTY_string
  | Tlist t -> TTY_list (type_translate t)
  | Ttuple ls -> TTY_tuple (List.map type_translate ls)


let rec isin x = function
  | [] -> false
  | (y,_)::t -> x=y || isin x t

(*adds a func to the type and the apply environment*)
let rec func_add e = match e.texpr_desc with
  | TE_fun (pat,e1,funid, fclos) when not @@ isin ("F"^string_of_int funid) !func_env -> 

    func_env :=
      ("F"^string_of_int funid,
        {constr_ty_params = List.map (fun x -> type_translate (snd x)) fclos;
         constr_ty_arg = (let (Tarrow(a,_)) = e.texpr_typ in type_translate a);
         constr_ty_ret = type_translate e1.texpr_typ})
      ::!func_env;

    let wut = texpr_translate e1 in (*tout bug si je ne le compute pas en dehors du :=  :( *)
    apply_env :=
      ("F"^string_of_int funid,List.map fst fclos,
       TE_let(false,tpatt_translate pat,arg,wut))
      ::!apply_env

  | _ -> ()

and texpr_translate (_ds : Tannot.texpr) : Target.tgt_expr = match _ds.texpr_desc with
  | TE_cte c -> 
      TE_cte c

  | TE_unop (op,e) -> 
      TE_unop (op,texpr_translate e)

  | TE_binop (op,e1,e2) -> 
      TE_binop (op,texpr_translate e1,texpr_translate e2)

  | TE_if (eb,e1,e2) -> 
      TE_if (texpr_translate eb, texpr_translate e1, texpr_translate e2)

  | TE_nil  -> 
      TE_nil

  | TE_cons (e1,e2) -> 
      TE_cons (texpr_translate e1, texpr_translate e2)

  | TE_app (e1,e2) -> 
      let e1' = texpr_translate e1 in
      if is_legit e1' then 
        TE_app (texpr_translate e1 , texpr_translate e2)
      else 
        TE_app ((TE_app (apply,texpr_translate e1)) , texpr_translate e2)

  | TE_tuple ls -> 
      TE_tuple (List.map texpr_translate ls)

  | TE_let (is_rec, pat, e1, e2) -> 
      TE_let ( is_rec, tpatt_translate pat,texpr_translate e1,texpr_translate e2)

  | TE_fun (_,_,funid, fclos) -> 
      func_add _ds;
      TE_func_constr ("F"^string_of_int funid, List.map fst fclos)

  | TE_ident x -> 
      TE_ident (ID_string x)

  | TE_match (e1, e2, (pat1, pat2, e3)) -> 
      TE_match (texpr_translate e1, texpr_translate e2,
                (tpatt_translate pat1, tpatt_translate pat2, texpr_translate e3))


                                                  
let tdef_translate ( (is_rec, pat, e) : Tannot.t_def) : tgt_def =
  (is_rec, tpatt_translate pat, texpr_translate e)   

let transl_file (_ds: t_def list): tgt_file =
  let defs = List.map tdef_translate _ds in
  let res =
  {tfile_func = !func_env;
   tfile_apply = !apply_env;
   tfile_defs = defs} 
  in func_env := [];apply_env := []; res
