open! Lang

let input_filename =
  match Sys.argv |> Array.to_list |> List.tl with
  | [file] -> file
  | _ -> Format.eprintf "usage: %s <input-file>\n" Sys.argv.(0); exit 1

let report_loc (b,e) =
  let l = b.Lexing.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.Lexing.pos_cnum - b.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" input_filename l fc lc

let () =
  let past =
    let cin = open_in input_filename in
    let lb = Lexing.from_channel cin in
    try
      let f = Parser.file Lexer.token lb in
      close_in cin;
      f
    with
    | Lexer.Lexical_error s ->
      report_loc (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb);
      Format.eprintf "lexical error: %s\n@." s;
      exit 1
    | Parsing.Parse_error ->
      report_loc (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb);
      Format.eprintf "syntax error\n@.";
      exit 1
  in
  let tast =
    try Typing.file past with
    | Typing.Error (l,e) ->
      report_loc l;
      Format.eprintf "%a\n@." Typing.report e;
      exit 1
  in
  let annotast = Tannotgen.file tast in
  let tgtast = Defunc.transl_file annotast in
  PPrint.ToChannel.pretty 1. 80 stdout (Targetpp.doc_file tgtast)
