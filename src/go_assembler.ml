
(* Assembleur de Petit Go *)

open Format
open X86_64
open Go_ast

module Smap = Map.Make(String)

type l_env = {
  env : (int * bool) Smap.t;
  next : int;
}

let lab_count = ref 0
let str_count = ref 0

let str_list = ref []

let s_env:((int * (Go_ast.raw_v_type * int) Smap.t) Smap.t ref) = ref Smap.empty

(* On redéfinit push et pop *)
let pop n = addq (imm (8*n)) !%rsp
let push n = subq (imm (8*n)) !%rsp

(* Crée un nouveau label *)
let new_lab () =
  incr lab_count;
  "__lab_"^(string_of_int !lab_count)

(* Stocke la chaîne dans .data et la nmérote *)
let new_str s =
  incr str_count;
  let lab_s = ".Str_"^(string_of_int !str_count) in
  str_list := (lab_s, s)::!str_list;
  lab lab_s

(* Quelle fonction utiliser pour l'affichage *)
let print_type = function
  | Tint -> "print_int"
  | Tbool -> "print_bool"
  | Tstring -> "print_string"
  | Tstruct s -> "print_struct_"^s
  | Pointer _ -> "print_pointer"
  | _ -> assert false

(* Renvoie la taille d'un type *)
let rec size = function
  | Tint | Tbool | Tstring | Pointer _ -> 1
  | Tstruct s -> fst (Smap.find s !s_env)
  | Tlist l -> List.fold_left (fun s t -> s + size t) 0 l
  | Tall -> print_string "Tall"; assert false
  | Tnil -> print_string "Tnil"; assert false

let size_8 t = 8 * (size t)

(* Crée l'environnement pour les variables locales à l'aide des paramètres *)
let rec create_env v =
  let _, env =
    List.fold_right
      (fun (l, t) (next, env) ->
         let s = 8 * size t in
         List.fold_right
           (fun (x:string) (n, e) ->
              n + s, Smap.add x ((n + s), true) e) l (next, env))
      v (8, Smap.empty) in
  {env=env; next=0}

(* Ajoute une variable locale à l'environnement *)
let add_loc l_env x t =
  let n = l_env.next + 8 in
  let env' = Smap.add x (-n, false) l_env.env in
  {env=env'; next=n}

(* On crée un élément nul *)
let init t pos =
  movq (imm (size t)) !%rsi ++
  movq (imm 8) !%rdi ++
  movq (imm 0) !%rax ++
  call "calloc" ++
  movq !%rax (ind ~ofs:(-pos) rbp)
(*
  let rec raz = function
    | 0 -> movq (imm 0) (ind ~ofs:(-pos) rbp)
    | i ->
      let p = pos + 8*i in
      movq (imm 0) (ind ~ofs:(-p) rbp) ++ (raz (i-1))
  in
  raz (s-1)
*)
(* Fonctions d'affichage *)
let rec call_print = function
  | Tint ->
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_int") rdi ++
    movq (imm 0) !%rax ++
    call "printf"
  | Tbool ->
    let lab_false = new_lab () in
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_false") rdi ++
    testq !%rsi !%rsi ++
    je lab_false ++
    leaq (lab ".Sprint_true") rdi ++
    label lab_false ++
    movq (imm 0) !%rax ++
    call "printf"
  | Tstring ->
    movq (imm 0) !%rax ++
    call "printf"
  | Tnil ->
    leaq (lab ".Sprint_nil") rdi ++
    movq (imm 0) !%rax ++
    call "printf"
  | Tstruct s -> nop (* TODO *)
  | Pointer (Tstruct s) ->
    let print_s = call_print (Tstruct s) in
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_amp") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    movq !%rsi !%rdi ++
    print_s
  | Pointer _ ->
    let lab_nil = new_lab () in
    let lab_ptr = new_lab () in
    testq !%rdi !%rdi ++
    je lab_nil ++
    movq !%rdi !%rsi ++
    leaq (lab ".Sprint_pointer") rdi ++
    jmp lab_ptr ++
    label lab_nil ++
    leaq (lab ".Sprint_nil") rdi ++
    label lab_ptr ++
    movq (imm 0) !%rax ++
    call "printf"
  | Tlist [] -> nop
  | Tlist [t] -> call_print t
  | Tlist l ->
    let size_l = size (Tlist l) in
    nop (* TODO avec les structures *)
  | Tall -> nop

(*let fmt_print v_env =
  (* TODO : print_strucures
     let rec print_fields
     let print_struct s =
     label ("print_struct_"^s) ++
     call "print_lbra" ++
     nop
     in
  *)
  fmt_print_base
*)
(* Arguments de printf pour l'affichage *)
let print_labels =
  label ".Sprint_int"     ++ string "%lld"   ++
  label ".Sprint_false"   ++ string "false"  ++
  label ".Sprint_true"    ++ string "true"   ++
  label ".Sprint_nil"     ++ string "<nil>"  ++
  label ".Sprint_pointer" ++ string "Ox%llx" ++
  label ".Sprint_newline" ++ string "\n"     ++
  label ".Sprint_space"   ++ string " "      ++
  label ".Sprint_lbra"    ++ string "{"      ++
  label ".Sprint_rbra"    ++ string "}"      ++
  label ".Sprint_amp"     ++ string "&"

(* Stockage des constantes *)
let compile_cst = function
  | Cint x -> movq (imm64 x) !%rdi
  | Cbool false -> movq (imm 0) !%rdi
  | Cbool true -> movq (imm 1) !%rdi
  | Cnil -> movq (imm 0) !%rdi
  | Cstring s -> leaq (new_str s) rdi

(* La valeur calculéee se trouve dans le registre %rdi *)
let rec compile_expr ?(keep_ptr=false) l_env = function
  | _, Tecst c ->
    compile_cst c
  | t, Tevar (x, _) ->
    let pos, t_val = Smap.find x l_env.env in
    movq (ind ~ofs:pos rbp) !%rdi ++
    (if size t = 1 && not keep_ptr && not t_val then
      (* On met directement la valeur dans %rdi *)
      movq (ind rdi) !%rdi
    else
      (* On garde l'adresse dans %rdi *)
      nop)
  | _, Teunop (u, e) ->
    compile_unop keep_ptr l_env e u
  | _, Tebinop (b, e1, e2) ->
    compile_binop l_env e1 e2 b
  | t, Tecall (f, l) ->
    let s, c =
      List.fold_left
        (fun (s, c) (t, _ as e) ->
           let c_e = compile_expr l_env e in
           if size t = 1 then
             s + 1, c ++ c_e ++ pushq !%rdi
           else
             size t, c ++ c_e) (0, nop) l in
    c ++
    call ("func_"^f) ++
    (if size t = 1 then movq !%rax !%rdi else nop) ++
    pop s
  | _, Tenew t ->
    movq (imm (size t)) !%rsi ++
    movq (imm 8) !%rdi ++
    movq (imm 0) !%rax ++
    call "calloc" ++
    movq !%rax !%rdi
  | _ -> assert false

(* Instructions unaires *)
and compile_unop keep_ptr l_env (t, _ as e) = function
  | Unot ->
    compile_expr l_env e ++
    cmpq !%rdi (imm 0) ++
    sete !%rdi
  | Uneg ->
    compile_expr l_env e ++
    negq !%rdi
  | Uref ->
    begin
      match t with
        | Pointer t' ->
          compile_expr l_env e ++
          (if size t' = 1 && not keep_ptr then movq (ind rdi) !%rdi else nop)
        | _ -> assert false
    end
  | Uderef ->
    compile_expr ~keep_ptr:true l_env e

(* Instructions pour les opérations binaires avec e1 dans %rdi et e2 dans %rsi
 * et place le résultat dans %rdi *)
and compile_binop l_env e1 e2 = function
  | Badd ->
    compile_expr l_env e2 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e1 ++
    addq !%rsi !%rdi
  | Bsub ->
    compile_expr l_env e2 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e1 ++
    subq !%rsi !%rdi
  | Bmul ->
    compile_expr l_env e2 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e1 ++
    imulq !%rsi !%rdi
  | Bdiv ->
    compile_expr l_env e1 ++
    movq !%rdi !%rax ++
    cqto ++
    compile_expr l_env e2 ++
    idivq !%rdi ++
    movq !%rax !%rdi
  | Bmod ->
    compile_expr l_env e1 ++
    movq !%rdi !%rax ++
    cqto ++
    compile_expr l_env e2 ++
    idivq !%rdi ++
    movq !%rdx !%rdi
  | Band ->
    let lab = new_lab () in
    compile_expr l_env e1 ++
    testq !%rdi !%rdi ++
    je lab ++
    (* If we didn't jump then only the value of e2 matters *)
    compile_expr l_env e2 ++
    label lab
  | Bor ->
    let lab = new_lab () in
    compile_expr l_env e1 ++
    testq !%rdi !%rdi ++
    jne lab ++
    (* If we didn't jump then only the value of e2 matters *)
    compile_expr l_env e2 ++
    label lab
  | Blt ->
    compile_expr l_env e1 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e2 ++
    cmpq !%rdi !%rsi ++
    setl !%dil ++
    andq (imm 1) !%rdi
  | Ble ->
    compile_expr l_env e1 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e2 ++
    cmpq !%rdi !%rsi ++
    setle !%dil ++
    andq (imm 1) !%rdi
  | Bgt ->
    compile_expr l_env e1 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e2 ++
    cmpq !%rdi !%rsi ++
    setg !%dil ++
    andq (imm 1) !%rdi
  | Bge ->
    compile_expr l_env e1 ++
    movq !%rdi !%rsi ++
    compile_expr l_env e2 ++
    cmpq !%rdi !%rsi ++
    setge !%dil ++
    andq (imm 1) !%rdi
  | _ -> assert false

(* Compilation d'un appel à print *)
let rec compile_print l_env = function
  | [] ->
    nop
  | [t, _ as e] ->
    compile_expr l_env e ++
    call_print t
  | (Tstring, _ as e)::q ->
    compile_expr l_env e ++
    call_print Tstring ++
    compile_print l_env q
  | (t, _ as e)::((Tstring, _)::_ as q) ->
    compile_expr l_env e ++
    call_print t ++
    compile_print l_env q
  | (t, _ as e)::q ->
    compile_expr l_env e ++
    call_print t ++
    leaq (lab ".Sprint_space") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    compile_print l_env q

(* On compile les instructions *)
let rec compile_instr l_env = function
  | Tiempty ->
    l_env, nop
  | Tibloc l ->
    List.fold_left
      (fun (e, c) i -> let e', c_i = compile_instr e i in e', c ++ c_i)
      (l_env, nop) (List.rev l)
  | Tiif (e, i1, i2) ->
    let lab_false = new_lab () and lab_true = new_lab () in
    let _, c1 = compile_instr l_env i1 in
    let _, c2 = compile_instr l_env i2 in
    l_env,
    compile_expr l_env e ++
    testq !%rdi !%rdi ++
    je lab_false ++
    c1 ++
    jmp lab_true ++
    label lab_false ++
    c2 ++
    label lab_true
  | Tiprint l ->
    l_env,
    compile_print l_env l
  | Tiexpr e ->
    l_env,
    compile_expr l_env e
  | Tiexec (t, f, l) ->
    let c = compile_expr l_env (t, Tecall (f, l)) in
    l_env,
    c ++
    (if size t = 1 then nop else pop (size t))
  | Tifor (e, i) ->
    let lab_end = new_lab () and lab_start = new_lab () in
    let _, c = compile_instr l_env i in
    l_env,
    label lab_start ++
    compile_expr l_env e ++
    testq !%rdi !%rdi ++
    je lab_end ++
    c ++
    jmp lab_start ++
    label lab_end
  | Tiassoc _ -> (* TODO *) assert false
  | Tivar (l, Some t, []) ->
    List.fold_left
      (fun (e, c) x ->
         let e' = add_loc e x t in
         e' , c ++ init t e'.next)
      (l_env, nop) l
  | Tivar (l, _, _) -> assert false
  | Tireturn [t, _ as e] when size t = 1 ->
    l_env,
    compile_expr l_env e ++
    movq !%rdi !%rax
  | Tireturn l -> assert false

(* On ne compile pas les structures, uniquement les fonctions *)
let compile_decl (f, v, t, b, s) =
  let l_env = create_env v in
  let _, c = compile_instr l_env b in
  let s' = if s = 0 then 0 else s + 2 - (s mod 2) in
  label ("func_"^f) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  push s' ++
  c ++
  pop s' ++
  popq rbp ++
  ret

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program (b, p, s, v_env, f_env) ofile =
  s_env := s;
  let code = List.map compile_decl p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
        globl "main" ++ label "main" ++
        call "func_main" ++
        movq (imm 0) !%rax ++
        ret ++
        code;
        (* On ne rajoute les instructions d'affichage que si fmt est importé *)
        (*(if b then fmt_print v_env else nop)*)
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
