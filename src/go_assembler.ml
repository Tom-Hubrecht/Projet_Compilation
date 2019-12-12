
(* Assembleur de Petit Go *)

open Format
open X86_64
open Go_ast

module Smap = Map.Make(String)

let lab_count = ref 0
let str_count = ref 0

let str_list = ref []

let new_lab () =
  incr lab_count;
  "__lab_"^(string_of_int !lab_count)

let new_str s =
  incr str_count;
  let lab_s = ".Str_"^(string_of_int !str_count) in
  str_list := (lab_s, s)::!str_list;
  lab lab_s

let print_type = function
  | Tint -> "print_int"
  | Tbool -> "print_bool"
  | Tstring -> "print_string"
  | Tstruct s -> "print_struct_"^s
  | Pointer _ -> "print_pointer"
  | _ -> assert false

let set_sizes s_env =
  let n_env = ref Smap.empty in
  let rec aux s =
    try fst (Smap.find s !n_env) with Not_found ->
      let fields = Smap.find s s_env in
      let f', size = Smap.fold
                   (fun x t (f, i) ->
                      match t with
                       | Tint | Tbool | Tstring | Pointer _ ->
                         (Smap.add x (t, i) f, i + 1)
                       | Tstruct s' ->
                         (Smap.add x (t, i) f, i + aux s')
                       | _ -> assert false) fields (Smap.empty, 0) in
      n_env := Smap.add s (size, f') !n_env;
      size
  in
  Smap.iter (fun s _ -> let _ = aux s in ()) s_env;
  !n_env

let get_size s_env = function
  | Tint | Tbool | Tstring | Pointer _ -> 1
  | Tstruct s -> fst (Smap.find s s_env)
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

  label "print_string" ++
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
  ret ++

  label "print_lbra" ++
  leaq (lab ".Sprint_space") rdi ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret ++

  label "print_rbra" ++
  leaq (lab ".Sprint_rbra") rdi ++
  movq (imm 0) !%rax ++
  call "printf" ++
  ret

let fmt_print s_env v_env =
  (* TODO : print_strucures
  let rec print_fields
  let print_struct s =
    label ("print_struct_"^s) ++
    call "print_lbra" ++
    nop
  in
  *)
  fmt_print_base

let print_labels =
  label ".Sprint_int" ++ string "%lld" ++
  label ".Sprint_false" ++ string "false" ++
  label ".Sprint_true" ++ string "true" ++
  label ".Sprint_nil" ++ string "<nil>" ++
  label ".Sprint_pointer" ++ string "Ox%lld" ++
  label ".Sprint_newline" ++ string "\n" ++
  label ".Sprint_space" ++ string " " ++
  label ".Sprint_lbra" ++ string "{" ++
  label ".Sprint_rbra" ++ string "}"

(* Stockage des constantes *)
let compile_cst = function
  | Cint x -> movq (imm64 x) !%rdi
  | Cbool false -> movq (imm 0) !%rdi
  | Cbool true -> movq (imm 1) !%rdi
  | Cnil -> movq (imm 0) !%rdi
  | Cstring s -> leaq (new_str s) rdi

(* La valeur calculéee se trouve dans le registre %rdi *)
let rec compile_expr = function
  | _, Tecst c ->
    compile_cst c
  | _, Teunop (u, e) ->
    compile_unop e u
  | _, Tebinop (b, e1, e2) ->
    compile_binop e1 e2 b
  | _ -> assert false

(* Instructions unaires *)
and compile_unop e = function
  | Unot ->
    compile_expr e ++
    cmpq !%rdi (imm 0) ++
    sete !%rdi
  | Uneg ->
    compile_expr e ++
    negq !%rdi
  | Uref
  | Uderef -> (* TODO *) assert false

(* Instructions pour les opérations binaires avec e1 dans %rdi et e2 dans %rsi
 * et place le résultat dans %rdi *)
and compile_binop e1 e2 = function
  | Badd ->
    compile_expr e2 ++
    movq !%rdi !%rsi ++
    compile_expr e1 ++
    addq !%rsi !%rdi
  | Bsub ->
    compile_expr e2 ++
    movq !%rdi !%rsi ++
    compile_expr e1 ++
    subq !%rsi !%rdi
  | Bmul ->
    compile_expr e2 ++
    movq !%rdi !%rsi ++
    compile_expr e1 ++
    imulq !%rsi !%rdi
  | Bdiv ->
    compile_expr e1 ++
    movq !%rdi !%rax ++
    cqto ++
    compile_expr e2 ++
    idivq !%rdi ++
    movq !%rax !%rdi
  | Bmod ->
    compile_expr e1 ++
    movq !%rdi !%rax ++
    cqto ++
    compile_expr e2 ++
    idivq !%rdi ++
    movq !%rdx !%rdi
  | Band ->
    let lab = new_lab () in
    compile_expr e1 ++
    testq !%rdi !%rdi ++
    je lab ++
    (* If we didn't jump then only the value of e2 matters *)
    compile_expr e2 ++
    label lab
  | Bor ->
    let lab = new_lab () in
    compile_expr e1 ++
    testq !%rdi !%rdi ++
    jne lab ++
    (* If we didn't jump then only the value of e2 matters *)
    compile_expr e2 ++
    label lab
  | Blt ->
    compile_expr e1 ++
    movq !%rdi !%rsi ++
    compile_expr e2 ++
    cmpq !%rdi !%rsi ++
    setl !%dil ++
    andq (imm 1) !%rdi
  | Ble ->
    compile_expr e1 ++
    movq !%rdi !%rsi ++
    compile_expr e2 ++
    cmpq !%rdi !%rsi ++
    setle !%dil ++
    andq (imm 1) !%rdi
  | Bgt ->
    compile_expr e1 ++
    movq !%rdi !%rsi ++
    compile_expr e2 ++
    cmpq !%rdi !%rsi ++
    setg !%dil ++
    andq (imm 1) !%rdi
  | Bge ->
    compile_expr e1 ++
    movq !%rdi !%rsi ++
    compile_expr e2 ++
    cmpq !%rdi !%rsi ++
    setge !%dil ++
    andq (imm 1) !%rdi
  | _ -> assert false

(* Compilation d'un appel à print *)
let rec compile_print = function
  | [] ->
    nop
  | [Tstring, _ as e] ->
    compile_expr e ++
    call "print_string"
  | [t, _ as e] ->
    compile_expr e ++
    call (print_type t)
  | (Tstring, _ as e)::q ->
    compile_expr e ++
    call "print_string" ++
    compile_print q
  | (t, _ as e)::((Tstring, _)::_ as q) ->
    compile_expr e ++
    call (print_type t) ++
    compile_print q
  | (t, _ as e)::q ->
    compile_expr e ++
    call (print_type t) ++
    call "print_space" ++
    compile_print q

(* On compile les instructions *)
let rec compile_instr = function
  | Tiempty ->
    nop
  | Tibloc l ->
    let b_code = List.rev (List.map compile_instr l) in
    List.fold_right (++) b_code nop
  | Tiif (e, i1, i2) ->
    let lab_false = new_lab () and lab_true = new_lab () in
    compile_expr e ++
    testq !%rdi !%rdi ++
    je lab_false ++
    compile_instr i1 ++
    jmp lab_true ++
    label lab_false ++
    compile_instr i2 ++
    label lab_true
  | Tiprint l ->
    compile_print l
  | Tiexpr e ->
    compile_expr e
  | Tiexec _ -> (* TODO *) assert false
  | Tiincr _ -> (* TODO *) assert false
  | Tidecr _ -> (* TODO *) assert false
  | Tifor (e, i) ->
    let lab_end = new_lab () and lab_start = new_lab () in
    label lab_start ++
    compile_expr e ++
    testq !%rdi !%rdi ++
    je lab_end ++
    compile_instr i ++
    jmp lab_start ++
    label lab_end
  | _ -> assert false

(* On ne compile pas les structures, uniquement les fonctions *)
let compile_decl = function
  | Tdfunc ((_, _, f), v, t, b) ->
    label ("func_"^f) ++
    compile_instr b ++
    ret
  | Tdstruct _ -> nop

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program (b, p, s_env, v_env, f_env) ofile =
  let s_env = set_sizes s_env in
  let code = List.map compile_decl p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
        globl "main" ++ label "main" ++ jmp "func_main" ++
        code ++
        ret ++
        (* On ne rajoute les instructions d'affichage que si fmt est importé *)
        (if b then fmt_print s_env v_env else nop);
      data =
        (*Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv*)
        List.fold_left (fun c (l, s) -> label l ++ string s ++ c )
          print_labels !str_list
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* on "flush" le buffer afin de s'assurer que tout y a été écrit
     avant de le fermer *)
  fprintf fmt "@?";
  close_out f
