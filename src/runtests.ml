open Lang

let tests_dir =
  match Sys.argv |> Array.to_list |> List.tl with
  | [dir] -> dir
  | _ -> Format.eprintf "usage: %s <tests_directory>\n" Sys.argv.(0); exit 1

let read_all cin =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec loop () =
    match input cin b 0 4096 with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf b 0 n; loop ()
  in
  loop ();
  Buffer.contents buf

let run_ocaml_on_file file =
  let stdout, stdin, stderr = Unix.open_process_args_full
      "ocaml" [|"ocaml"; file|]
      (Unix.environment ())
  in
  let output = read_all stdout in
  let err = read_all stderr in
  ignore @@ Unix.close_process_full (stdout, stdin, stderr);
  output, err

let run_ocaml_on_doc (doc: PPrint.document) =
  let stdout, stdin, stderr = Unix.open_process_args_full
      "ocaml" [|"ocaml"; "-stdin"|]
      (Unix.environment ())
  in
  PPrint.ToChannel.pretty 1. 80 stdin doc;
  close_out stdin;
  let output = read_all stdout in
  let err = read_all stderr in
  ignore @@ Unix.close_process_full (stdout, stdin, stderr);
  output, err

let report_loc (b,e) fmt =
  let l = b.Lexing.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.Lexing.pos_cnum - b.pos_bol + 1 in
  Format.fprintf fmt "Line %d, characters %d-%d:" l fc lc

let (let*) = Result.bind

type test_output = {
  original : PPrint.document;
  output : string; err : string;
  reference_output : string; reference_err : string;
}

type test_error =
  | TestExn of (Format.formatter -> unit)
  | TestOutput of test_output

let runtest file : (string, test_error) result =
  let* past =
    let cin = open_in file in
    let lb = Lexing.from_channel cin in
    try
      let f = Parser.file Lexer.token lb in
      close_in cin;
      Ok f
    with
    | Lexer.Lexical_error s ->
      let loc = (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb) in
      Error (TestExn (fun fmt ->
        report_loc loc fmt;
        Format.fprintf fmt " Lexical error@,%s" s
      ))
    | Parsing.Parse_error ->
      let loc = (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb) in
      Error (TestExn (fun fmt ->
        report_loc loc fmt;
        Format.fprintf fmt " Syntax error";
      ))
    | exn ->
      let exn_s = Printexc.to_string exn in
      Error (TestExn (fun fmt -> Format.fprintf fmt "%s" exn_s))
  in
  let* tast =
    try Ok (Typing.file past) with
    | Typing.Error (l,e) ->
      Error (TestExn (fun fmt ->
        report_loc l fmt;
        Format.fprintf fmt " Type error@,%a" Typing.report e
      ))
  in
  let* tgtast =
    try
      let annotast = Tannotgen.file tast in
      Ok (Defunc.transl_file annotast)
    with exn ->
      let exn_s = Printexc.to_string exn in
      Error (TestExn (fun fmt -> Format.fprintf fmt "%s" exn_s))
  in
  let reference_output, reference_err = run_ocaml_on_file file in
  let output, err = run_ocaml_on_doc (Targetpp.doc_file tgtast) in
  if output = reference_output &&
     err = reference_err
  then Ok output
  else Error (TestOutput {
    original = Targetpp.doc_file tgtast;
    reference_output; reference_err;
    output; err
  })

let green s = "\027[1;32m" ^ s ^ "\027[0m"
let red s = "\027[1;31m" ^ s ^ "\027[0m"

let main () =
  let testfiles =
    Sys.readdir tests_dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".ml")
    |> List.sort String.compare
    |> List.map (fun file -> Filename.concat tests_dir file)
  in
  List.iter (fun file ->
    Printf.printf "Testing %s... %!" file;
    match runtest file with
    | Ok _output ->
      Printf.printf "%s\n%!" (green "[OK]")

    | Error (TestExn pp) ->
      Printf.printf "%s\n%!" (red "[ERROR]");
      Format.printf "@[<v>";
      Format.printf "Error when processing the input program:@,";
      Format.printf "@[<v>%t@]@," pp;
      Format.printf "@]@."
    | Error (TestOutput { original; output; err;
                          reference_output; reference_err }) ->
      Printf.printf "%s\n%!" (red "[ERROR]");
      Printf.printf "Mismatch between source and output program.\n";
      Printf.printf "Output program.\n";
      PPrint.ToChannel.compact stdout original;
      Printf.printf "- Running 'ocaml' on source program produces:\n";
      Printf.printf "%s\n%s\n" reference_err reference_output;
      Printf.printf "- Running 'ocaml' on output program produces:\n";
      Printf.printf "%s\n%s\n" err output;
  ) testfiles

let () = main ()
