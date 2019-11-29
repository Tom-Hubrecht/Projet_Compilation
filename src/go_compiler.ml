
(* Fichier principal du compilateur de Petit Go *)

open Lexing
open Format
open Go_printer

let parse_only = ref false
let type_only = ref false
let print_tree = ref false

let () = Format.set_margin 80

let usage = "usage: pgoc [option] file.go"

let spec =
  [
    "--parse-only", Arg.Set parse_only, " stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing";
    "-p", Arg.Set print_tree, " displays the ast"
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
  eprintf "'%s' : " s

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Go_parser.file Go_lexer.token lb in
    close_in c;
    if !print_tree then print_file f;
    if !parse_only then exit 0;
    let _ = Go_typer.check_file f in
    if !type_only then exit 0;
    exit 0;
  with
    | Go_lexer.Lexing_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb, lexeme lb);
      eprintf "lexical error: %s.@." s;
      exit 1
    | Go_parser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb, lexeme lb);
      eprintf "syntax error.@.";
      exit 1
    | Go_typer.Typing_error((s_p, e_p, _) as e, r1, r2) ->
      report (s_p, e_p, (p_str p_expr e));
      eprintf "this expression has type %a but is expected to have type %a.@."
        p_r_type r1 p_r_type r2;
      exit 1
    | Go_typer.Decl_error (i, s) ->
      report i;
      eprintf "%s@." s;
      exit 1
    | Go_typer.Field_error ((_, _, i) as e, t) ->
      report e;
      eprintf "the type %a does not have a field %s.@." p_type t i;
      exit 1
    | Go_typer.Nil_error ((sp, ep, _) as e) ->
      report (sp, ep, (p_str p_expr e));
      eprintf "the operation nil==nil is not defined.@.";
      exit 1
    | Go_typer.Left_error ((sp, ep, _) as e) ->
      report (sp, ep, (p_str p_expr e));
      eprintf "This expression is not a left value.@.";
      exit 1
    | Go_typer.Import_error s ->
      eprintf "%s@." s;
      exit 1
    | Go_typer.Main_error s ->
      eprintf "%s@." s;
      exit 1
    | Go_typer.Length_expr_error (k, l, sp, ep) ->
      report (sp, ep, (p_str (p_list ", " p_expr) l));
      eprintf "%d values expected but %d given." k (List.length l);
      exit 1
    | Go_typer.Recursive_error s ->
      report s;
      eprintf "this structure has a recursive definition.@.";
      exit 1
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2


