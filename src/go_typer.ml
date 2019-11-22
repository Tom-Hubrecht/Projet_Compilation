
(* Analyse du typage d'un fichier Petit Go *)

open Go_ast

exception Typing_error of expr * raw_r_type * raw_r_type
exception Decl_error of ident * string
exception Field_error of ident * raw_v_type
exception Nil_error of expr
exception Left_error of expr

module Smap = Map.Make(String)

let str_of_expr = Go_printer.p_str Go_printer.p_expr
let str_of_instr = Go_printer.p_str Go_printer.p_instr

let a_decl = " is already declared"

let get_opt = function
  | None -> raise (Invalid_argument "None has no content")
  | Some x -> x

let content_of_loc (_, _, x) = x

let id_of_type = function
  | s, e, Tstruct t -> s, e, t
  | _ -> assert false

let rec check_type v = function
  | sp, ep, Pointer t -> check_type v (sp, ep, t)
  | t ->
    if List.mem (content_of_loc t) v then
      content_of_loc t
    else
      raise (Decl_error (id_of_type t, "Undefined structure."))

let check_o_type v = function
  | None -> None
  | Some t -> Some (check_type v t)

let rec check_r_type v = function
  | [] -> []
  | t::q ->
    let q' = check_r_type v q in
    (check_type v t)::q'

let rec get_var_type v = function
  | [] -> Smap.empty
  | (l, t)::q ->
    let t' = check_type v t in
    List.fold_left
      (fun m x ->
         let x' = content_of_loc x in
         if Smap.mem x' m then
           raise (Decl_error (x, "This var"^a_decl))
         else
           Smap.add x' t' m)
      (get_var_type v q) l

let type_cst = function
  | Cint x -> Tint, Cint x
  | Cstring s -> Tstring, Cstring s
  | Cbool b -> Tbool, Cbool b
  | Cnil -> Tnil, Cnil

let rec type_left_expr f_env s_env env = function
  | _, _, Evar "_" -> (Tnoop, Tevar "_")
  | sp, ep, Evar x ->
      if Smap.mem x env then
        (Smap.find x env), Tevar x
      else
        raise (Decl_error ((sp, ep, x), "Undefined variable."))
  | _, _, Eattr(e, _) as e1 ->
      let _ = type_left_expr f_env s_env env e in
      type_expr f_env s_env env e1
  | e -> raise (Left_error e)

and type_expr f_env s_env env = function
  | _, _, Ecst x -> let t, c = type_cst x in t, Tecst x
  | sp, ep, Evar x ->
      if Smap.mem x env then
        (Smap.find x env), Tevar x
      else
        raise (Decl_error ((sp, ep, x), "Undefined variable."))
  | _, _, Eattr(e, i) ->
      begin
        let (t, _) as e' = type_expr f_env s_env env e in
        match t with
          | Tstruct s | Pointer(Tstruct s) ->
              begin
                let s' = Smap.find s s_env in
                let i' = content_of_loc i in
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
              if t1 <> Tint then
                raise (Typing_error (e1, [t1], [Tbool]));
              if t2 <> Tint then
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
                  | Tnil, Pointer _ | Pointer _, Tnil | _, _ when t1 = t2 ->
                      (Tbool, Tebinop(b, e1', e2'))
                  | Tnil, Tnil -> raise (Nil_error e)
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
  | (_, _, Ecall _) as e -> type_call f_env s_env env e
  | sp, ep, Eprint _ as e ->
      let i = sp, ep, str_of_expr e in
      raise (Decl_error (i, "fmt.Print has no type and cannot be used here."))

and type_instr f_env s_env env = function
  | _, _, Iempty -> Tiempty
  | _, _, Ibloc l -> Tibloc (List.map (type_instr f_env s_env env) l)
  | _, _, Iif(e, i1, i2) ->
      let (t, _) as e' = type_expr f_env s_env env e in
      if t <> Tbool then
        raise (Typing_error (e, [t], [Tbool]));
      Tiif(e', type_instr f_env s_env env i1, type_instr f_env s_env env i2)
  | _, _, Iexpr(_, _, Eprint l) ->
      Tiprint (List.map (type_expr f_env s_env env) l)
  | _, _, Iexpr e -> Tiexpr(type_expr f_env s_env env e)
  | _, _, Iincr e ->
      let (t, _) as e' = type_left_expr f_env s_env env e in
      if t <> Tint then
        raise (Typing_error (e, [t], [Tint]));
      Tiincr(e')
  | _, _, Idecr e ->
      let (t, _) as e' = type_left_expr f_env s_env env e in
      if t <> Tint then
        raise (Typing_error (e, [t], [Tint]));
      Tidecr(e')
  | _, _, Ifor(e, i) ->
      let (t, _) as e' = type_expr f_env s_env env e in
      if t <> Tbool then
        raise (Typing_error (e, [t], [Tbool]));
      Tifor(e', type_instr f_env s_env env i)
  | _, _, Iassoc(l1, [_, _, Ecall _]) -> assert false
  | sp, ep, Iassoc(l1, l2) as i->
      if List.length l1 <> List.length l2 then
        raise (Decl_error
                 ((sp, ep, str_of_instr i),
                  "both sides of = must have the same number of expressions"));
      let l1' = List.map (type_expr f_env s_env env) l1 in
      let l2' = List.map (type_expr f_env s_env env) l2 in
      Tiassoc(l1', l2')
  | _, _, Ivar(l1, None, l2) -> assert false
  | _, _, Ireturn l -> assert false
  | _ -> assert false

and type_call f_env s_env env = function
  | _, _, Ecall((_, _, "new"), [sp, ep, Evar s]) ->
      if Smap.mem s s_env then
        Pointer (Tstruct s), Tenew s
      else
        raise (Decl_error ((sp, ep, s), "Undefined type"))
  | sp, ep, Ecall((_, _, "new"), _) as e ->
      let s = str_of_expr e in
      raise (Decl_error ((sp, ep, s), "new must be called on a structure"))
  | _ -> assert false

let check_file (fmt_imported, l) =
  (* On ajoute toutes les structures *)
  let add_struct l = function
    | Dstruct(s, _) ->
        let s' = content_of_loc s in
        if List.mem (Tstruct s') l then
          raise (Decl_error (s, "This structure"^a_decl));
        (Tstruct s')::l
    | Dfunc _ -> l
  in
  let v_types = List.fold_left add_struct [Tint; Tstring; Tbool] l in
  (* On ajoute les fonctions et les champs de structure *)
  let add_env (f_m, s_m) = function
    | Dfunc(f, v, t, _) ->
        let f' = content_of_loc f in
        if Smap.mem f' f_m then
          raise (Decl_error (f, "This function"^a_decl));
        let vars = get_var_type v_types v in
        let t = check_r_type v_types t in
        Smap.add f' (t, vars) f_m, s_m
    | Dstruct(s, v) ->
        let s' = content_of_loc s in
        let vars = get_var_type v_types v in
        f_m, Smap.add s' vars s_m
  in
  let f_env, s_env = List.fold_left add_env (Smap.empty, Smap.empty) l in
  let check_func = function
    | Dfunc(f, v, t, b) ->
        let b' = type_instr f_env s_env
                   (snd (Smap.find (content_of_loc f) f_env)) b in
        Tdfunc(f, v, t, b')
    | Dstruct(s, v) -> Tdstruct(s, v)
  in
  let l' = List.map check_func l in
  let check_recur s_env =
    ()
  in
  check_recur s_env;
  (fmt_imported, l')

