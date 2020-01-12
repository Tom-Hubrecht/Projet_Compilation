
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

let s_env = ref Smap.empty

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
  | Tint | Tbool | Tstring | Pointer _ | Tnil -> 1
  | Tstruct s -> fst (Smap.find s !s_env)
  | Tlist l -> List.fold_left (fun s t -> s + size t) 0 l
  | Tall -> print_string "Tall"; assert false

let size_8 t = 8 * (size t)

(* Renvoie la liste ordonnée des champs *)
let get_ordered_fields s =
  let rec ins (_, o as f) = function
    | [] -> [f]
    | (_, o' as f')::q ->
      if o < o' then
        f::f'::q
      else
        f'::(ins f q)
  in
  Smap.fold (fun _ f l -> ins f l) (snd (Smap.find s !s_env)) []

(* Renvoie le type et le décalage d'un champ du type t *)
let get_field f = function
  | Pointer (Tstruct s) | Tstruct s ->
    Smap.find f (snd (Smap.find s !s_env))
  | _ -> assert false

(* Crée l'environnement pour les variables locales à l'aide des paramètres *)
let rec create_env v =
  let _, env =
    List.fold_right
      (fun (l, t) (next, env) ->
         let s = size_8 t in
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

(* Fonctions d'affichage *)
let rec call_print ?(follow_ptr=true) = function
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
  | Tstruct s when size (Tstruct s) = 0 ->
    leaq (lab ".Sprint_lbra") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    leaq (lab ".Sprint_rbra") rdi ++
    movq (imm 0) !%rax ++
    call "printf"
  | Tstruct s ->
    let fields = get_ordered_fields s in
    let rec p_struct = function
      | [] -> nop
      | [t, ofs] ->
        popq rdi ++
        addq (imm ofs) !%rdi ++
        (if size t = 1 then movq (ind rdi) !%rdi else nop) ++
        call_print ~follow_ptr:false t
      | (t, ofs)::q ->
        let c = p_struct q in
        popq rdi ++
        pushq !%rdi ++
        addq (imm ofs) !%rdi ++
        (if size t = 1 then movq (ind rdi) !%rdi else nop) ++
        call_print ~follow_ptr:false t ++
        leaq (lab ".Sprint_space") rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        c
    in
    pushq !%rdi ++
    leaq (lab ".Sprint_lbra") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    p_struct fields ++
    leaq (lab ".Sprint_rbra") rdi ++
    movq (imm 0) !%rax ++
    call "printf"
  | Pointer (Tstruct s) when follow_ptr ->
    let print_s = call_print ~follow_ptr:false (Tstruct s) in
    let lab_nil = new_lab () in
    let lab_ptr = new_lab () in
    testq !%rdi !%rdi ++
    je lab_nil ++
    pushq !%rdi ++
    leaq (lab ".Sprint_amp") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    popq rdi ++
    print_s ++
    jmp lab_ptr ++
    label lab_nil ++
    leaq (lab ".Sprint_nil") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    label lab_ptr
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
  | Tlist l ->
    (*let size_l = size (Tlist l) in *)
    nop (* TODO avec les structures *)
  | Tall -> nop

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
let rec compile_expr ?(k_p=false) l_env = function
  | _, Tecst c ->
    compile_cst c
  | t, Tevar (x, _, pos) ->
    let p, t_val = Smap.find x l_env.env in
    let pos = (if t_val then p else pos) in
    if k_p && t_val then
      leaq (ind ~ofs:pos rbp) rdi
    else
      begin
        movq (ind ~ofs:pos rbp) !%rdi ++
        (if size t = 1 && not k_p && not t_val then
           (* On met directement la valeur dans %rdi *)
           movq (ind rdi) !%rdi
         else
           (* On garde l'adresse dans %rdi *)
           nop)
      end
  | _, Teunop (u, e) ->
    compile_unop k_p l_env e u
  | _, Tebinop (b, e1, e2) ->
    compile_binop l_env e1 e2 b
  | t, Tecall (f, l) ->
    let s, c = compile_call l_env t f l in
    c ++ pop s ++
    movq !%rax !%rdi
  | t, Tecomp (f, e) ->
    let s, c = compile_comp l_env t f e in
    c ++ pop s ++
    movq !%rax !%rdi
  | _, Tenew t ->
    movq (imm (size t)) !%rsi ++
    movq (imm 8) !%rdi ++
    movq (imm 0) !%rax ++
    call "calloc" ++
    movq !%rax !%rdi
  | _, Teattr ((Pointer t, Tevar (x, _, pos)), f) ->
    let p, t_val = Smap.find x l_env.env in
    let t', ofs = get_field f t in
    if t_val then
      begin
        if size t' = 1 && not k_p then
          movq (ind ~ofs:p rbp) !%rdi ++
          movq (ind ~ofs:ofs rdi) !%rdi
        else
          movq (ind ~ofs:p rbp) !%rdi ++
          addq (imm ofs) !%rdi
      end
    else
      movq (ind ~ofs:pos rbp) !%rdi ++
      movq (ind rdi) !%rdi ++
      addq (imm ofs) !%rdi ++
      (if size t' = 1 && not k_p then
         movq (ind rdi) !%rdi
       else
         nop)
  | _, Teattr ((Pointer t, _ as e), f) ->
    let t', ofs = get_field f t in
    let c = compile_expr l_env e in
    c ++
    addq (imm ofs) !%rdi ++
    (if size t' = 1 && not k_p then
       movq (ind rdi) !%rdi
     else
       nop)
  | _, Teattr ((t, Tevar (x, _, pos)), f) ->
    let p, t_val = Smap.find x l_env.env in
    let t', ofs = get_field f t in
    if t_val then
      begin
        if size t' = 1 && not k_p then
          movq (ind ~ofs:(p + ofs) rbp) !%rdi
        else
          leaq (ind ~ofs:(p + ofs) rbp) rdi
      end
    else
      movq (ind ~ofs:pos rbp) !%rdi ++
      addq (imm ofs) !%rdi ++
      (if size t' = 1 && not k_p then
         movq (ind rdi) !%rdi
       else
         nop)
  | _, Teattr ((t', _ as e), f) ->
    let c = compile_expr ~k_p:k_p l_env e in
    let (t, ofs) = get_field f t' in
    c ++
    addq (imm ofs) !%rdi ++
    (if size t = 1 && not k_p then
       movq (ind rdi) !%rdi
     else
       nop)

(* Compilation des appels de fonction *)
and compile_call l_env t f l =
  let s, c =
    List.fold_left
      (fun (s, c) (t, _ as e) ->
         let c_e = compile_expr l_env e in
         if size t = 1 then
           s + 1, c ++ c_e ++ pushq !%rdi
         else
           s + size t,
           c ++ c_e ++
           push (size t) ++
           movq !%rdi !%rsi ++
           movq !%rsp !%rdi ++
           movq (imm (size_8 t)) !%rdx ++
           call "memmove")
      (0, nop) l in
  s,
  c ++
  call ("func_"^f)

and compile_comp l_env t1 f (t, _ as e) =
  let c_e = compile_expr l_env e in
  let s = size t in
  s,
  c_e ++
  push s ++
  movq !%rdi !%rsi ++
  movq !%rsp !%rdi ++
  movq (imm (size_8 t)) !%rdx ++
  call "memmove" ++
  call ("func_"^f)
  (*let s, c =
    List.fold_left
      (fun (s, c) (t, _ as e) ->
         let c_e = compile_expr l_env e in
         if size t = 1 then
           s + 1, c ++ c_e ++ pushq !%rdi
         else
           s + size t,
           c ++ c_e ++
           push (size t) ++
           movq !%rdi !%rsi ++
           movq !%rsp !%rdi ++
           movq (imm (size_8 t)) !%rdx ++
           call "memmove")
      (0, nop) l in
  s,
  c ++
  call ("func_"^f)*)

and compile_diff l_env eq (t, _ as e1) e2 =
  if size t = 1 then
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    cmpq !%rdi !%rsi ++
    (if eq then sete else setne) !%dil ++
    andq (imm 1) !%rdi
  else
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    movq (imm (size_8 t)) !%rdx ++
    call "memcmp" ++
    testq !%rax !%rax ++
    (if eq then sete else setne) !%dil ++
    andq (imm 1) !%rdi


(* Instructions unaires *)
and compile_unop keep_ptr l_env (t, _ as e) = function
  | Unot ->
    compile_expr l_env e ++
    testq !%rdi !%rdi ++
    sete !%dil ++
    andq (imm 1) !%rdi
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
    compile_expr ~k_p:true l_env e

(* Instructions pour les opérations binaires avec e1 dans %rdi et e2 dans %rsi
 * et place le résultat dans %rdi *)
and compile_binop l_env e1 e2 = function
  | Badd ->
    compile_expr l_env e2 ++
    pushq !%rdi ++
    compile_expr l_env e1 ++
    popq rsi ++
    addq !%rsi !%rdi
  | Bsub ->
    compile_expr l_env e2 ++
    pushq !%rdi ++
    compile_expr l_env e1 ++
    popq rsi ++
    subq !%rsi !%rdi
  | Bmul ->
    compile_expr l_env e2 ++
    pushq !%rdi ++
    compile_expr l_env e1 ++
    popq rsi ++
    imulq !%rsi !%rdi
  | Bdiv ->
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rax ++
    cqto ++
    idivq !%rdi ++
    movq !%rax !%rdi
  | Bmod ->
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rax ++
    cqto ++
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
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    cmpq !%rdi !%rsi ++
    setl !%dil ++
    andq (imm 1) !%rdi
  | Ble ->
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    cmpq !%rdi !%rsi ++
    setle !%dil ++
    andq (imm 1) !%rdi
  | Bgt ->
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    cmpq !%rdi !%rsi ++
    setg !%dil ++
    andq (imm 1) !%rdi
  | Bge ->
    compile_expr l_env e1 ++
    pushq !%rdi ++
    compile_expr l_env e2 ++
    popq rsi ++
    cmpq !%rdi !%rsi ++
    setge !%dil ++
    andq (imm 1) !%rdi
  | Beq ->
    compile_diff l_env true e1 e2
  | Bneq ->
    compile_diff l_env false e1 e2

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
let rec compile_instr l_env ret_lab = function
  | Tiempty ->
    l_env, nop
  | Tibloc l ->
    List.fold_left
      (fun (e, c) i -> let e', c_i = compile_instr e ret_lab i in e', c ++ c_i)
      (l_env, nop) (List.rev l)
  | Tiif (e, i1, i2) ->
    let lab_false = new_lab () and lab_true = new_lab () in
    let _, c1 = compile_instr l_env ret_lab i1 in
    let _, c2 = compile_instr l_env ret_lab i2 in
    l_env,
    compile_expr l_env e ++
    testq !%rdi !%rdi ++
    je lab_false ++
    c1 ++
    jmp lab_true ++
    label lab_false ++
    c2 ++
    label lab_true
  | Tiprint [Tlist r, Tecall (f, l)] -> l_env, nop
  | Tiprint l ->
    l_env,
    compile_print l_env l
  | Tiexpr e ->
    l_env,
    compile_expr l_env e
  | Tiexec (t, f, l) ->
    let s, c_f = compile_call l_env t f l in
    l_env,
    c_f ++ pop s
  | Tifor (e, i) ->
    let lab_end = new_lab () and lab_start = new_lab () in
    let _, c = compile_instr l_env ret_lab i in
    l_env,
    label lab_start ++
    compile_expr l_env e ++
    testq !%rdi !%rdi ++
    je lab_end ++
    c ++
    jmp lab_start ++
    label lab_end
  | Tiassoc (l1, [Tlist r, Tecomp (f, l)]) ->
    let s, c_f = compile_comp l_env (Tlist r) f l in
    l_env,
    List.fold_right
      (fun (t, _ as e) c ->
         c ++
         compile_expr ~k_p:true l_env e ++
         popq rsi ++
         (if size t = 1 then
            movq !%rsi (ind rdi)
          else
            movq (imm (size_8 t)) !%rdx ++
            call "memmove"))
      l1 (c_f ++ pop s ++ pushq !%rax)
  | Tiassoc (l1, [Tlist r, Tecall (f, l)]) ->
    let s, c_f = compile_call l_env (Tlist r) f l in
    l_env,
    List.fold_right
      (fun (t, _ as e) c ->
         c ++
         compile_expr ~k_p:true l_env e ++
         popq rsi ++
         (if size t = 1 then
            movq !%rsi (ind rdi)
          else
            movq (imm (size_8 t)) !%rdx ++
            call "memmove"))
      l1 (c_f ++ pop s ++ pushq !%rax)
  | Tiassoc (l1, l2) ->
    let c_d =
      List.fold_left
        (fun c e -> c ++ (compile_expr l_env e) ++ pushq !%rdi)
        nop l2 in
    let c_g =
      List.fold_right
        (fun (t, _ as e) c ->
           c ++
           (compile_expr ~k_p:true l_env e) ++
           popq rsi ++
           (if size t = 1 then
              movq !%rsi (ind rdi)
            else
              movq (imm (size_8 t)) !%rdx ++
              call "memmove"))
        l1 nop in
    l_env, c_d ++ c_g
  | Tivar (l, Some t, []) ->
    List.fold_left
      (fun (e, c) x ->
         let e' = add_loc e x t in
         e' , c ++ init t e'.next)
      (l_env, nop) l
  | Tivar ([x], None, [t, _ as e]) ->
    let l_env' = add_loc l_env x t in
    let pos = l_env'.next in
    l_env',
    compile_expr l_env e ++
    pushq !%rdi ++
    init t pos ++
    popq rsi ++
    (if size t = 1 then
       movq !%rsi (ind rax)
     else
       movq !%rax !%rdi ++
       movq (imm (size_8 t)) !%rdx ++
       call "memmove")
  | Tivar (l1, None, [Tlist r, Tecomp (f, l)]) ->
    let s, c_f = compile_comp l_env (Tlist r) f l in
    let n_env, c =
      List.fold_right2
        (fun x t (e, c) ->
           let e' = add_loc e x t in
           e',
           c ++
           init t e'.next ++
           popq rsi ++
           (if size t = 1 then
              leaq (ind ~ofs:8 rsi) rbx ++
              pushq !%rbx ++
              movq (ind rsi) !%rsi ++
              movq !%rsi (ind rax)
            else
              leaq (ind ~ofs:(size_8 t) rsi) rbx ++
              pushq !%rbx ++
              movq !%rax !%rdi ++
              movq (imm (size_8 t)) !%rdx ++
              call "memmove"))
        l1 r (l_env, c_f ++ pop s ++ pushq !%rax) in
    n_env,
    c ++ pop 1
  | Tivar (l1, None, [Tlist r, Tecall (f, l)]) ->
    let s, c_f = compile_call l_env (Tlist r) f l in
    let n_env, c =
      List.fold_right2
        (fun x t (e, c) ->
           let e' = add_loc e x t in
           e',
           c ++
           init t e'.next ++
           popq rsi ++
           (if size t = 1 then
              leaq (ind ~ofs:8 rsi) rbx ++
              pushq !%rbx ++
              movq (ind rsi) !%rsi ++
              movq !%rsi (ind rax)
            else
              leaq (ind ~ofs:(size_8 t) rsi) rbx ++
              pushq !%rbx ++
              movq !%rax !%rdi ++
              movq (imm (size_8 t)) !%rdx ++
              call "memmove"))
        l1 r (l_env, c_f ++ pop s ++ pushq !%rax) in
    n_env,
    c ++ pop 1
  | Tivar (l1, None, l2) ->
    let c_d, l_t =
      List.fold_left
        (fun (c, l) (t, _ as e) ->
           (compile_expr l_env e) ++ pushq !%rdi ++ c, t::l)
        (nop, []) l2 in
    List.fold_right2
      (fun x t (e, c) ->
         let e' = add_loc e x t in
         e',
         c ++
         init t e'.next ++
         popq rsi ++
         (if size t = 1 then
            movq !%rsi (ind rax)
          else
            movq !%rax !%rdi ++
            movq (imm (size_8 t)) !%rdx ++
            call "memmove"))
      l1 (List.rev l_t) (l_env, c_d)
  | Tivar (l, _, _) -> assert false
  | Tireturn [] -> l_env, jmp ret_lab
  | Tireturn [t, _ as e] when size t = 1 ->
    l_env,
    compile_expr l_env e ++
    movq !%rdi !%rax ++
    jmp ret_lab
  | Tireturn l ->
    let c_r, s_r =
      List.fold_left
        (fun (c, s) (t, _ as e) ->
           let c_e = c ++
                     pushq !%rax ++
                     compile_expr l_env e ++
                     popq rax in
           (if size t = 1 then
              c_e ++ movq !%rdi (ind ~ofs:s rax), s + 8
            else
              c_e ++
              pushq !%rax ++
              movq !%rdi !%rsi ++
              leaq (ind ~ofs:s rax) rdi ++
              movq (imm (size_8 t)) !%rdx ++
              call "memmove",
              s + size_8 t))
        (nop, 0) l in
    l_env,
    movq (imm s_r) !%rdi ++
    movq (imm 0) !%rax ++
    call "malloc" ++
    c_r ++
    jmp ret_lab

(* On ne compile pas les structures, uniquement les fonctions *)
let compile_decl (f, v, t, b, s) =
  let l_env = create_env v in
  let lab_ret = "ret_"^f in
  let _, c = compile_instr l_env lab_ret b in
  let s' = if s = 0 then 0 else s + 2 - (s mod 2) in
  label ("func_"^f) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  push s' ++
  c ++
  label lab_ret ++
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
