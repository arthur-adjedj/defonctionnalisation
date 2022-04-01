(* pretty printing *)
open Asttypes
open Target
open PPrint

let rec doc_ctyp typ =
  doc_ctyp_tuple typ
and doc_ctyp_tuple = function
  | TTY_tuple tys ->
    group (
      separate (space ^^ star ^^ break 1)
        (List.map doc_ctyp_app tys)
    )
  | ty -> doc_ctyp_app ty
and doc_ctyp_app = function
  | TTY_list ty -> doc_ctyp_app ty ^//^ string "list"
  | TTY_func (ty1, ty2) ->
    group (parens (
      align (doc_ctyp ty1 ^^ comma ^^ break 1 ^^ doc_ctyp ty2))) ^//^
    string "func"
  | ty -> doc_ctyp_atom ty
and doc_ctyp_atom = function
  | TTY_var id -> string id
  | TTY_int -> string "int"
  | TTY_unit -> string "unit"
  | TTY_bool -> string "bool"
  | TTY_string -> string "string"
  | (TTY_func _ | TTY_list _ | TTY_tuple _) as ty ->
    group (parens (doc_ctyp ty))

let rec doc_patt = function
  | TP_any -> string "_"
  | TP_ident id -> string id
  | TP_tuple [] -> assert false
  | TP_tuple [p] -> doc_patt p
  | TP_tuple ps ->
    group (parens (
      align (
        separate (comma ^^ break 1)
          (List.map doc_patt ps)
      )
    ))

let doc_constant = function
  | Cunit -> string "()"
  | Cbool b -> OCaml.bool b
  | Cint n -> OCaml.int n
  | Cstring s -> OCaml.string s

let doc_unop = function
  | Unot -> string "not"
  | Uminus -> string "-"

let doc_binop = function
  | Beq -> string "="
  | Bneq -> string "<>"
  | Blt -> string "<"
  | Ble -> string "<="
  | Bgt -> string ">"
  | Bge -> string ">="
  | Badd -> string "+"
  | Bsub -> string "-"
  | Bmul -> string "*"
  | Bdiv -> string "/"
  | Band -> string "&&"
  | Bor -> string "||"
  | Bconcat -> string "^"

let doc_cstr cname params =
  prefix 2 0 (string cname)
    (match params with
     | [] -> empty
     | [x] -> break 1 ^^ string x
     | ids -> break 1 ^^ doc_patt (TP_tuple (List.map (fun id -> TP_ident id) ids)))

let rec reconstruct_expr_list = function
  | TE_cons (e1, e2) ->
    (match reconstruct_expr_list e2 with
     | None -> None
     | Some es -> Some (e1 :: es))
  | TE_nil -> Some []
  | _ -> None

let rec reconstruct_app = function
  | TE_app (e1, e2) ->
    reconstruct_app e1 @ [e2]
  | e -> [e]

let doc_op op doc1 doc2 =
  infix 2 1 (doc_binop op) doc1 doc2

let rec doc_expr e =
  doc_expr_ifmatch e
and doc_expr_ifmatch = function
  | TE_if (cond, e1, e2) ->
    group (align (
      group (string "if" ^^ jump 2 1 (doc_expr cond) ^/^ string "then") ^^
      jump 2 1 (doc_expr e1) ^/^
      string "else" ^^
      jump 2 1 (doc_expr e2)
    ))
  | TE_match (e, enil, (p1, p2, econs)) ->
    group (align (
      group (string "match" ^^ jump 2 1 (doc_expr e) ^/^ string "with") ^^ hardline ^^
      group (string "| [] ->" ^^ jump 2 1 (doc_expr enil)) ^^ hardline ^^
      group (
        group (string "| " ^^
               align (group (doc_patt p1 ^^ space ^^ string "::") ^/^
                      group (doc_patt p2 ^^ string " ->"))) ^^
        jump 2 1 (doc_expr econs)
      ) ^^ hardline
    ))
  | e -> doc_expr_let e

and doc_expr_let = function
  | TE_let (is_rec, p, e1, e2) ->
    group (
      group (align (
        hang 2 (flow (break 1) [
          string (if is_rec then "let rec" else "let");
          doc_patt p;
          string "=";
          doc_expr_ifmatch e1
        ]) ^/^
        string "in")) ^/^
      doc_expr_let e2
    )
  | e -> doc_expr_binop e

and doc_expr_binop e =
  doc_expr_binop_or e
and doc_expr_binop_or = function
  | TE_binop (Bor, e1, e2) ->
    doc_op Bor (doc_expr_binop_or e1) (doc_expr_binop_and e2)
  | e -> doc_expr_binop_and e
and doc_expr_binop_and = function
  | TE_binop (Band, e1, e2) ->
    doc_op Band (doc_expr_binop_and e1) (doc_expr_binop_comp e2)
  | e -> doc_expr_binop_comp e
and doc_expr_binop_comp = function
  | TE_binop ((Blt | Ble | Bgt | Bge | Bneq | Beq) as op, e1, e2) ->
    doc_op op (doc_expr_binop_comp e1) (doc_expr_binop_concat e2)
  | e -> doc_expr_binop_concat e
and doc_expr_binop_concat = function
  | TE_binop (Bconcat, e1, e2) ->
    doc_op Bconcat (doc_expr_binop_cons e1) (doc_expr_binop_concat e2)
  | e -> doc_expr_binop_cons e
and doc_expr_binop_cons = function
  | TE_cons (e1, e2) as e ->
    begin match reconstruct_expr_list e with
      | Some _ -> doc_expr_binop_plus e
      | None ->
        doc_expr_binop_plus e1 ^^ space ^^ string "::" ^/^
        doc_expr_binop_cons e2
    end
  | e -> doc_expr_binop_plus e
and doc_expr_binop_plus = function
  | TE_binop ((Badd | Bsub) as op, e1, e2) ->
    doc_op op (doc_expr_binop_plus e1) (doc_expr_binop_star e2)
  | e -> doc_expr_binop_star e
and doc_expr_binop_star = function
  | TE_binop ((Bmul | Bdiv) as op, e1, e2) ->
    doc_op op (doc_expr_binop_star e1) (doc_expr_unop e2)
  | e -> doc_expr_unop e

and doc_expr_unop = function
  | TE_unop (Uminus, e) -> group (doc_unop Uminus ^//^ doc_expr_app e)
  | e -> doc_expr_app e

and doc_expr_app = function
  | TE_app (_, _) as e ->
    group (hang 2 (separate (break 1) (
      List.map doc_expr_simple @@ reconstruct_app e)))
  | TE_unop (Unot, e) ->
    group (prefix 2 1 (doc_unop Unot) (doc_expr_simple e))
  | TE_func_constr (cname, ids) ->
    group (doc_cstr cname ids)
  | e -> doc_expr_simple e

and doc_expr_simple = function
  | TE_cte c -> doc_constant c
  | TE_ident ID_apply -> string "__apply"
  | TE_ident ID_arg -> string "__arg"
  | TE_ident (ID_string s) -> string s
  | TE_tuple es ->
    group (parens (align (
      separate (comma ^^ break 1)
        (List.map doc_expr es)
    )))
  | TE_func_constr (cname, []) -> string cname
  | TE_nil -> string "[]"
  | TE_cons _ as e ->
    begin match reconstruct_expr_list e with
      | Some es ->
        group (brackets (align (
          separate (semi ^^ break 1)
            (List.map doc_expr es)
        )))
      | None ->
        group (parens (doc_expr e))
    end
  | _ as e ->
    group (parens (doc_expr e))

let doc_def (is_rec, p, e) =
  group (
    group (
      hang 2 (flow (break 1) [
        string (if is_rec then "let rec" else "let");
        doc_patt p;
        string "=";
      ]) ^//^
      doc_expr e
    ) ^^ hardline
  )

let doc_defs ds =
  separate hardline (List.map doc_def ds)

let doc_func cs =
  if cs = [] then empty else begin
    group (hang 2 (
      string "type (_, _) func =" ^^ hardline ^^
      separate hardline (List.map (fun (cname, cty) ->
        group (hang 2 (
          group (string "| " ^^ string cname ^^ string " :" ^^
                 (match cty.constr_ty_params with
                  | [] -> empty
                  | tys -> break 1 ^^ doc_ctyp (TTY_tuple tys) ^/^ string "->")) ^/^
          group (parens (align (doc_ctyp cty.constr_ty_arg ^^ string "," ^/^
                                doc_ctyp cty.constr_ty_ret)) ^/^ string "func")
        ))
      ) cs
      ) ^^ hardline
    ))
  end

let doc_apply cases =
  if cases = [] then empty else begin
    group (
      string "[@@@ocaml.warning \"-26\"]" ^^ hardline ^^
      group (hang 2 (flow (break 1) [
        string "let rec __apply :";
        string "type a r. (a, r) func -> a -> r =";
        string "fun f __arg ->"
      ])) ^^
      jump 2 0 (
        string "match f with" ^^ hardline ^^
        separate hardline (List.map (fun (cname, ids, e) ->
          group (hang 2 (
            group (string "| " ^^ doc_cstr cname ids ^/^ string "->") ^/^
            doc_expr e
          ))
        ) cases
        ) ^^ hardline
      )
    )
  end

let doc_file file =
  doc_func file.tfile_func ^^ hardline ^^
  doc_apply file.tfile_apply ^^ hardline ^^
  doc_defs file.tfile_defs
