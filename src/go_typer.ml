
(* Analyse du typage d'un fichier Petit Go *)

open Go_ast
open Go_printer
open Format

exception Typing_error of expr * raw_r_type * raw_r_type
exception Decl_error of ident * string
exception Field_error of ident * raw_v_type
exception Nil_error of expr
exception Left_error of expr
exception Import_error of string
exception Main_error of string

module Smap = Map.Make(String)

let str_of_expr = Go_printer.p_str Go_printer.p_expr
let str_of_instr = Go_printer.p_str Go_printer.p_instr

let a_decl = " is already declared"

let d_pos = Lexing.dummy_pos

let id_of_type = function
  | s, e, Tstruct t -> s, e, t
  | _ -> assert false

let rec check_type v = function
  | sp, ep, Pointer t -> Pointer (check_type v (sp, ep, t))
  | _, _, t' as t->
    if List.mem t' v then
      t'
    else
      raise (Decl_error (id_of_type t, "Undefined structure."))

let rec check_r_type v = function
  | [] -> []
  | t::q ->
    let q' = check_r_type v q in
    (check_type v t)::q'

(* returns an Smap and a r_type representing the input types *)
let rec get_var_type v = function
  | [] -> [], Smap.empty
  | (l, t)::q ->
    let t' = check_type v t in
    List.fold_left
      (fun (l', m) x ->
         let _, _, x' = x in
         if Smap.mem x' m then
           raise (Decl_error (x, "This var"^a_decl))
         else
           t'::l', Smap.add x' t' m)
      (get_var_type v q) l

(* checks that a value of type t2 can be assigned to a value of type t1 *)
let compatible = function
  | Pointer _, Tnil
  | Tall, _ -> true
  | t1, t2 when t1 = t2 -> true
  | _ -> false

let type_cst = function
  | Cint x -> Tint, Cint x
  | Cstring s -> Tstring, Cstring s
  | Cbool b -> Tbool, Cbool b
  | Cnil -> Tnil, Cnil

let add_var (sp, ep, x' as x) t env =
  if x' <> "_" then
    begin
      if Smap.mem x' !env then
        raise (Decl_error (x, "this variable is already declared"));
      env := Smap.add x' (t, false, (sp, ep)) !env
    end

let rec type_left_expr f_env s_env env = function
  | _, _, Evar "_" -> (Tall, Tevar "_")
  | sp, ep, Evar x ->
    if Smap.mem x !env then
      begin
        let t, _, p = Smap.find x !env in
        env := Smap.add x (t, true, p) !env;
        t, Tevar x
      end
    else
      raise (Decl_error ((sp, ep, x), "undefined variable."))
  | _, _, Eattr(e, _) as e1 ->
    let _ = type_left_expr f_env s_env env e in
    type_expr f_env s_env env e1
  | _, _, Eunop(Uref, _) as e -> type_expr f_env s_env env e
  | e -> raise (Left_error e)

and type_expr f_env s_env env = function
  | _, _, Ecst x -> let t, c = type_cst x in t, Tecst x
  | sp, ep, Evar "_" ->
    raise (Decl_error ((sp, ep, "_"), "cannot use _ as a value"))
  | sp, ep, Evar x ->
    if Smap.mem x !env then
      begin
        let t, _, p = Smap.find x !env in
        env := Smap.add x (t, true, p) !env;
        t, Tevar x
      end
    else
      raise (Decl_error ((sp, ep, x), "undefined variable."))
  | _, _, Eattr(e, (_, _, i' as i)) ->
    begin
      let (t, _) as e' = type_expr f_env s_env env e in
      match t with
        | Tstruct s | Pointer(Tstruct s) ->
          begin
            let s' = Smap.find s s_env in
            try
              let t' = Smap.find i' s' in
              (t', Teattr(e', i'))
            with
              | Not_found -> raise (Field_error (i, t))
          end
        | _ -> raise (Field_error (i, t))
    end
  | (_, _, Ebinop(b, e1, e2)) as e ->
    begin
      let (t1, _) as e1' = type_expr f_env s_env env e1 in
      let (t2, _) as e2' = type_expr f_env s_env env e2 in
      match b with
        | Badd | Bsub | Bmul | Bdiv | Bmod ->
          if t1 <> Tint then
            raise (Typing_error (e1, [t1], [Tint]));
          if t2 <> Tint then
            raise (Typing_error (e2, [t2], [Tint]));
          (Tint, Tebinop(b, e1', e2'))
        | Band | Bor ->
          if t1 <> Tbool then
            raise (Typing_error (e1, [t1], [Tbool]));
          if t2 <> Tbool then
            raise (Typing_error (e2, [t2], [Tbool]));
          (Tbool, Tebinop(b, e1', e2'))
        | Blt | Ble | Bgt | Bge ->
          if t1 <> Tint then
            raise (Typing_error (e1, [t1], [Tint]));
          if t2 <> Tint then
            raise (Typing_error (e2, [t2], [Tint]));
          (Tbool, Tebinop(b, e1', e2'))
        | Beq | Bneq ->
          begin
            match t1, t2 with
              | Tnil, Tnil -> raise (Nil_error e)
              | Tnil, Pointer _ | Pointer _, Tnil | _, _ when t1 = t2 ->
                (Tbool, Tebinop(b, e1', e2'))
              | _ -> raise (Typing_error (e1, [t1], [t2]));
          end
    end
  | _, _, Eunop(u, e) ->
    begin
      let (t, _) as e' = type_expr f_env s_env env e in
      match u, t with
        | Uneg, Tint -> Tint, Teunop(u, e')
        | Unot, Tbool -> Tbool, Teunop(u, e')
        | Uref, Pointer t' -> t', Teunop(u, e')
        | Uderef, t' when t' <> Tnil ->
          let _ = type_left_expr f_env s_env env e in
          Pointer t', Teunop(u, e')
        | _ -> raise (Typing_error (e, [t], [Tnil]))
    end
  | (sp, ep, Ecall(f, l)) as e ->
    begin
      let r, e' = type_call f_env s_env env (f, l) in
      match r with
        | [] -> Tall, e'
        | [t] -> t, e'
        | _ ->
          raise (Decl_error
                   ((sp, ep, str_of_expr e),
                    "this function should return only one value"))
    end
  | sp, ep, Eprint _ as e ->
    let i = sp, ep, str_of_expr e in
    raise (Decl_error (i, "fmt.Print has no type and cannot be used here."))

(* type_instr returns the new AST & true if a value is returned,
 * false otherwise *)
and type_instr f_env s_env env r v fmt = function
  | _, _, Iempty ->
    false, false, Tiempty
  | _, _, Ibloc l ->
    let b = ref false and u = ref false in
    let l' = List.fold_left
               (fun l' i ->
                  let b', u', i' = type_instr f_env s_env env r v fmt i in
                  b := !b || b';
                  u := !u || u';
                  i'::l') [] l in
    Smap.iter
      (fun x (_, b, (sp, ep)) ->
         if not b then
           raise (Decl_error((sp, ep, x),
                             "variable "^x^" declared but not used")))
      !env;
    !b, !u, Tibloc l'
  | _, _, Iif(e, i1, i2) ->
    let (t, _) as e' = type_expr f_env s_env env e in
    if t <> Tbool then
      raise (Typing_error (e, [t], [Tbool]));
    let env1 = ref !env and env2 = ref !env in
    let b1, u1, i1' = type_instr f_env s_env env1 r v fmt i1 in
    let b2, u2, i2' = type_instr f_env s_env env2 r v fmt i2 in
    b1 && b2, u1 || u2, Tiif(e', i1', i2')
  | _, _, Iexpr(sp, ep, Eprint l as e) ->
    if not fmt then
      raise (Decl_error ((sp, ep, str_of_expr e), "fmt not imported."));
    false, true, Tiprint (List.map (type_expr f_env s_env env) l)
  | _, _, Iexpr e ->
    false, false, Tiexpr (type_expr f_env s_env env e)
  | _, _, Iincr e ->
    let (t, _) as e' = type_left_expr f_env s_env env e in
    if t <> Tint then
      raise (Typing_error (e, [t], [Tint]));
    false, false, Tiincr e'
  | _, _, Idecr e ->
    let (t, _) as e' = type_left_expr f_env s_env env e in
    if t <> Tint then
      raise (Typing_error (e, [t], [Tint]));
    false, false, Tidecr e'
  | _, _, Ifor(e, i) ->
    let (t, _) as e' = type_expr f_env s_env env e in
    if t <> Tbool then
      raise (Typing_error (e, [t], [Tbool]));
    let _, u, i' = type_instr f_env s_env env r v fmt i in
    false, u, Tifor (e', i')
  | _, _, Iassoc(l1, [sp, ep, Ecall (f, l) as e]) ->
    let r, e' = type_call f_env s_env env (f, l) in
    if List.length l1 <> List.length r then
      raise (Decl_error ((sp, ep, str_of_expr e),
                         "this function is expected to return"^
                            (string_of_int (List.length l1)^"values.")));
    (* TODO compute new lists *)
    false, false, Tiassoc([], [])
  | sp, ep, Iassoc(l1, l2) as i ->
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of = must have the same number of expressions."));
    let l1', l2' = List.fold_left2
      (fun (l1'', l2'') e1 e2 ->
         let (t1, _) as e1' = type_left_expr f_env s_env env e1 in
         let (t2, _) as e2' = type_expr f_env s_env env e2 in
         if not (compatible (t1, t2)) then
           raise (Typing_error (e2, [t2], [t2]));
         e1'::l1'', e2'::l2'') ([], []) l1 l2 in
    false, false, Tiassoc(l1', l2')
  | _, _, Ivar(l1, None, [_, _, Ecall _]) -> assert false
  | sp, ep, Ivar(l1, None, l2) as i ->
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of := must have the same number of expressions."));
    let l' = List.fold_left2
               (fun l x (sp', ep', _ as e) ->
                  let (t, _) as e' = type_expr f_env s_env env e in
                  if t = Tnil then
                    raise (Decl_error ((sp', ep', str_of_expr e),
                                       "use of untyped nil."));
                  add_var x t env;
                  e'::l) [] l1 l2 in
    false, false, Tivar(l1, None, List.rev l')
  | _, _, Ivar(l1, Some t, [_, _, Ecall _]) -> assert false
  | _, _, Ivar(l1, Some t, []) ->
    let t' = check_type v t in
    List.iter (fun x -> add_var x t' env) l1;
    false, false, Tivar(l1, Some t, [])
  | sp, ep, Ivar(l1, Some t, l2) as i ->
    let t = check_type v t in
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of := must have the same number of expressions."));
    let l' = List.fold_left2
               (fun l x e ->
                  let (t', _) as e' = type_expr f_env s_env env e in
                  if not (compatible (t, t')) then
                    raise (Typing_error (e, [t'], [t]));
                  add_var x t env;
                  e'::l) [] l1 l2 in
    false, false, Tivar(l1, None, List.rev l')
  | _, _, Ireturn [_, _, Ecall _] -> assert false
  | sp, ep, Ireturn l as e_r ->
    if (List.length l <> List.length r) then
      raise (Decl_error ((sp, ep, str_of_instr e_r),
                         (string_of_int (List.length r))^
                         " values must be returned"));
    let l' = List.fold_left2
               (fun lis t1 e ->
                  let (t2, _) as e' = type_expr f_env s_env env e in
                  if t1 <> t2 then
                    raise (Typing_error (e, [t2], [t1]));
                  e'::lis) [] r l in
    true, false, Tireturn (List.rev l')

(* type_call returns the list of types returned and the transformed AST *)
and type_call f_env s_env env = function
  | (_, _, "new"), [sp, ep, Evar s] ->
    begin
      match s with
        | "int" -> [Pointer Tint], Tenew Tint
        | "bool" -> [Pointer Tbool], Tenew Tbool
        | "string" -> [Pointer Tstring], Tenew Tstring
        | _ ->
          if Smap.mem s s_env then
            [Pointer (Tstruct s)], Tenew (Tstruct s)
          else
            raise (Decl_error ((sp, ep, s), "Undefined type"));
    end
  | (sp, ep, "new") as f, e ->
    let s = str_of_expr (sp, ep, Ecall(f, e)) in
    raise (Decl_error ((sp, ep, s), "new must be called on a structure."));
  | (_, _, f') as f, [_, _, Ecall(g, l) as e] ->
    if not (Smap.mem f' f_env) then
      raise (Decl_error (f, "undefined function."));
    let t, eg = type_call f_env s_env env (g, l) in
    let r, a, _ = Smap.find f' f_env in
    if a = [] then
      raise (Decl_error (g, "unexpected argument."));
    if List.length t <> List.length a then
      raise (Typing_error (e, t, r));
    List.iter2
      (fun t1 t2 -> if t1 <> t2 then raise (Typing_error (e, t, r))) t a;
    r, Tecomp (f', eg)
   | (_, _, f') as f, l ->
    if not (Smap.mem f' f_env) then
      raise (Decl_error (f, "undefined function."));
    let r, a, _ = Smap.find f' f_env in
    if List.length l <> List.length a then
      raise
        (Decl_error (f, (string_of_int (List.length r))^" values expected."));
    let l' = List.fold_left2
               (fun lis t1 e ->
                  let t2, e' = type_expr f_env s_env env e in
                  if t1 <> t2 then
                    raise (Typing_error (e, [t2], [t1]));
                  e'::lis) [] a l in
    r, Tecall (f', (List.rev l'))

let check_file (fmt, l) =
  (* On ajoute toutes les structures *)
  let add_struct l = function
    | Dstruct(s, _) ->
      let _, _, s' = s in
      if List.mem (Tstruct s') l then
        raise (Decl_error (s, "This structure"^a_decl));
      (Tstruct s')::l
    | Dfunc _ -> l
  in
  let v_types = List.fold_left add_struct [Tint; Tstring; Tbool] l in
  (* On ajoute les fonctions et les champs de structure *)
  let add_env (f_m, s_m) = function
    | Dfunc(f, v, t, _) ->
      let _, _, f' = f in
      if Smap.mem f' f_m then
        raise (Decl_error (f, "This function"^a_decl));
      let r, vars = get_var_type v_types v in
      let t = check_r_type v_types t in
      Smap.add f' (t, r, vars) f_m, s_m
    | Dstruct(s, v) ->
      let _, _, s' = s in
      let _, vars = get_var_type v_types v in
      f_m, Smap.add s' vars s_m
  in
  let f_env, s_env = List.fold_left add_env (Smap.empty, Smap.empty) l in
  if not (Smap.mem "main" f_env) then
    raise (Main_error "a main function must be declared.");
  let r, _, a = Smap.find "main" f_env in
  if r <> [] then
    raise (Main_error "main function cannot return anything.");
  if a <> Smap.empty then
    raise (Main_error "main function cannot take arguments.");
  let fmt' = ref false in
  let check_func = function
    | Dfunc((_, _, f') as f, v, t, b) ->
      let r, t_a, env = Smap.find f' f_env in
      let env' = Smap.map (fun x -> x, true, (d_pos, d_pos)) env in
      let ret, fu, b' = type_instr f_env s_env (ref env') r v_types fmt b in
      fmt' := !fmt' || fu;
      if r != [] && not ret then
        raise (Decl_error (f, "this function must return a value."));
      Tdfunc(f, v, t, b')
    | Dstruct(s, v) -> Tdstruct(s, v)
  in
  let l' = List.map check_func l in
  if fmt && (not !fmt') then
    raise (Import_error "fmt imported but not used.");
  let check_recur s_env =
    ()
  in
  check_recur s_env;
  l'

