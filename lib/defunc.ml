open Tannot
open Target

let todo = failwith "TODO"

let rec tpatt_translate (pat : Tannot.tpatt)  : Target.tgt_patt = match pat.tpatt_desc with
  | TP_any -> TP_any
  | TP_ident x -> TP_ident x
  | TP_tuple ls -> TP_tuple (List.map tpatt_translate ls) 

let rec type_translate = function
  | Tvar _ -> todo
  | Tarrow (a, b)-> (match type_translate b with
    |TTY_func (TTY_tuple ls, t2) -> TTY_func (TTY_tuple ((type_translate a)::ls), t2)
    |TTY_func (t1, t2) -> TTY_func (TTY_tuple [type_translate a; t1], t2)
    |_ -> TTY_func (type_translate a, type_translate b)
    )
  | Tint -> TTY_int
  | Tunit -> TTY_unit
  | Tbool -> TTY_bool
  | Tstring -> TTY_string
  | Tlist t -> TTY_list (type_translate t)
  | Ttuple ls -> TTY_tuple (List.map type_translate ls)

let rec texpr_translate (_ds : Tannot.texpr) : Target.tgt_expr = match _ds.texpr_desc with
| TE_cte c -> TE_cte c
| TE_unop (op,e) -> TE_unop (op,texpr_translate e)
| TE_binop (op,e1,e2) -> TE_binop (op,texpr_translate e1,texpr_translate e2)
| TE_if (eb,e1,e2) -> TE_if (texpr_translate eb, texpr_translate e1, texpr_translate e2)
| TE_nil  -> TE_nil
| TE_cons (e1,e2) -> TE_cons (texpr_translate e1, texpr_translate e2)
| TE_app (e1,e2) -> TE_app (texpr_translate e1, texpr_translate e2)
| TE_tuple ls -> TE_tuple (List.map texpr_translate ls)
| TE_let (is_rec, pat, e1, e2) -> TE_let (is_rec, tpatt_translate pat,texpr_translate e1,texpr_translate e2)
| TE_fun (*(pat,e,funid,fclos)*) _ -> todo
| TE_ident _ -> todo
| TE_match (e1, e2, (pat1, pat2, e3)) -> TE_match (texpr_translate e1, texpr_translate e2,
                                                  (tpatt_translate pat1, tpatt_translate pat2, texpr_translate e3))


(*add stuff to an env here, no idea what yet*)
let tdef_translate ( (is_rec, pat, e) : Tannot.t_def) : tgt_def =
  (is_rec, tpatt_translate pat, texpr_translate e)   

let transl_file (_ds: t_def list): tgt_file =
  {tfile_func = todo;
   tfile_apply = todo;
   tfile_defs = List.map tdef_translate _ds}
