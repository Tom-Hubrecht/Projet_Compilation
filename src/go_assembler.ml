
(* Assembleur de Petit Go *)

open Format
open X86_64
open Go_ast

let print_type = function
  | Tint -> "print_int"
  | Tbool -> "print_bool"
  | Tstring -> "print_string"
  | Tstruct s -> "print_"^s
  | Pointer _ -> "print_pointer"
  | _ -> assert false

(* Instructions d'affichage de base *)
let fmt_print_base =
  label "print_int" ++
  movq !%rdi !%rsi ++
  leaq (lab ".Sprint_int") rdi ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret ++

  label "print_bool" ++
  movq !%rdi !%rsi ++
  leaq (lab ".Sprint_false") rdi ++
  testq !%rsi !%rsi ++
  je "_p_false" ++
  leaq (lab ".Sprint_true") rdi ++
  label "_p_false" ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret ++

  label "print_newline" ++
  leaq (lab ".Sprint_newline") rdi ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret ++

  label "print_space" ++
  leaq (lab ".Sprint_space") rdi ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret

let fmt_print s_env =
  (*let print_struct s =
    label ("print_"^s) ++
    nop
  in*)
  fmt_print_base

let print_labels =
  label ".Sprint_int" ++ string "%d" ++
  label ".Sprint_false" ++ string "false" ++
  label ".Sprint_true" ++ string "true" ++
  label ".Sprint_nil" ++ string "<nil>" ++
  label ".Sprint_pointer" ++ string "Ox%d" ++
  label ".Sprint_newline" ++ string "\n" ++
  label ".Sprint_space" ++ string " "

(* Instructions pour les opérations binaires avec e1 dans %rdi et e2 dans %rsi
 * et place le résultat dans %rdi *)
let compile_binop = function
  | Badd -> addq !%rsi !%rdi
  | Bsub -> subq !%rsi !%rdi
  | Bmul -> imulq !%rsi !%rdi
  | Bdiv ->
    movq !%rdi !%rax ++
    movq (imm 0) !%rdx ++
    idivq !%rsi ++
    movq !%rax !%rdi
  | Bmod ->
    movq !%rdi !%rax ++
    movq (imm 0) !%rdx ++
    divq !%rsi ++
    movq !%rdx !%rdi
  | _ -> assert false

(* Stockage des constantes *)
let compile_cst = function
  | Cint x -> movq (imm64 x) !%rdi
  | Cbool false -> movq (imm 0) !%rdi
  | Cbool true -> movq (imm 1) !%rdi
  | Cnil -> movq (imm 0) !%rdi
  | _ -> assert false

(* La valeur calculéee se trouve dans le registre %rdi *)
let rec compile_expr = function
  | _, Tecst c ->
    compile_cst c
  | _, Tebinop (b, e1, e2) ->
    compile_expr e2 ++
    movq !%rdi !%rsi ++
    compile_expr e1 ++
    compile_binop b
  | _ -> assert false

(* Compilation d'un appel à print *)
let rec compile_print = function
  | [] ->
    call "print_newline"
  | (t, _ as e)::q ->
    (compile_expr e) ++
    call (print_type t) ++
    call "print_space" ++
    compile_print q

(* On compile les instructions *)
let rec compile_instr = function
  | Tiempty -> nop
  | Tibloc l ->
    let b_code = List.map compile_instr l in
    List.fold_right (++) b_code nop
  | Tiexpr e -> compile_expr e
  | Tiprint l -> compile_print l
  | _ -> assert false

(* On ne compile pas les structures, uniquement les fonctions *)
let compile_decl = function
  | Tdfunc ((_, _, f), v, t, b) ->
    label ("func_"^f) ++
    compile_instr b
  | Tdstruct _ -> nop

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program (b, p, s_env, f_env) ofile =
  let code = List.map compile_decl p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
        globl "main" ++ label "main" ++
        nop (* À COMPLÉTER *) ++
        code ++
        ret ++
        (* On ne rajoute les instructions d'affichage que si fmt est importé *)
        (if b then fmt_print s_env else nop);
      data =
        (*Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv*)
          (print_labels)
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* on "flush" le buffer afin de s'assurer que tout y a été écrit
     avant de le fermer *)
  fprintf fmt "@?";
  close_out f
