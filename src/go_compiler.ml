
(* Fichier principal du compilateur de Petit Go *)

open Lexing
open Format

let parse_only = ref false
let type_only = ref false
let print_tree = ref false

let usage = "usage: pgoc [option] file.go"

let spec =
  [
    "--parse-only", Arg.Set parse_only, " stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing";
    "--print-tree", Arg.Set print_tree, " displays the ast"
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".go") then
      raise (Arg.Bad "no .go extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None -> Arg.usage spec usage; exit 1

let report (b, e, s) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc;
  eprintf "\t%S --> " s

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Go_parser.file Go_lexer.token lb in
    close_in c;
    if !print_tree then Go_printer.print_file f;
    if !parse_only then exit 0;
  with
  | Go_lexer.Lexing_error s ->
    report (lexeme_start_p lb, lexeme_end_p lb, lexeme lb);
    eprintf "lexical error: %s@." s;
    exit 1
  | Go_parser.Error ->
    report (lexeme_start_p lb, lexeme_end_p lb, lexeme lb);
    eprintf "syntax error@.";
    exit 1
  | e ->
    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2


