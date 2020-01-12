
(* Analyse du typage d'un fichier Petit Go *)

open Go_ast
open Format

exception Typing_error of expr * raw_r_type * raw_r_type
exception Decl_error of ident * string
exception Field_error of ident * raw_v_type
exception Recursive_error of ident
exception Nil_error of expr
exception Left_error of expr
exception Import_error of string
exception Main_error of string
exception Length_expr_error of int * expr list *
                               Lexing.position * Lexing.position
exception Length_ident_error of int * ident list *
                                Lexing.position * Lexing.position

module Smap = Map.Make(String)

let str_of_expr = Go_printer.p_str Go_printer.p_expr
let str_of_instr = Go_printer.p_str Go_printer.p_instr
let str_of_type = Go_printer.p_str Go_printer.p_type

let a_decl = " is already declared."

let d_pos = Lexing.dummy_pos

(* Checks if a structure has a recursive definition *)
let check_recur s_env p_env =
  let rec aux s vis =
    if Smap.mem s vis then
      begin
        if Smap.find s vis then
          vis
        else
          let sp, ep = Smap.find s p_env in
          raise (Recursive_error (sp, ep, s))
      end
    else
      let f, _ = Smap.find s s_env in
      let m = Smap.fold
                (fun x s v ->
                   match s with
                     | Tstruct s' -> aux s' v
                     | _ -> v)
                f (Smap.add s false vis) in
      Smap.add s true m
  in
  Smap.fold (fun s _ v -> aux s v) s_env Smap.empty

(* Linéarise une liste de variables *)
let rec flatten_var l =
  let rec aux acc t = function
    | [] -> acc
    | (_, _, v)::q -> (v, t)::(aux acc t q)
  in
  match l with
    | [] -> []
    | (l, (_, _, t))::q -> aux (flatten_var q) t l

(* Calcule la taille de tous les types *)
let get_sizes s_env =
  let n_env = ref Smap.empty in
  let rec aux s =
    try fst (Smap.find s !n_env) with Not_found ->
      let fields, l_var = Smap.find s s_env in
      let f', size =
        List.fold_left
          (fun (f, i) (x, t) ->
             match t with
               | Tint | Tbool | Tstring | Pointer _ ->
                 (Smap.add x (t, i) f, i + 8)
               | Tstruct s' ->
                 (Smap.add x (t, i) f, i + 8 * (aux s'))
               | _ -> assert false)
          (Smap.empty, 0) (flatten_var l_var) in
      n_env := Smap.add s (size, f') !n_env;
      size
  in
  Smap.iter (fun s _ -> let _ = aux s in ()) s_env;
  !n_env

(* Returns the size of a type *)
let rec size s_env = function
  | Tint | Tbool | Tstring | Pointer _ -> 1
  | Tstruct s -> fst (Smap.find s s_env)
  | Tlist r -> List.fold_left (fun s t -> s + size s_env t) 0 r
  | _ -> assert false

(* Extracts the type from a string *)
let type_of_str (_, _, s as s') s_env =
  let n = String.length s in
  let rec aux i = match s.[i] with
    | '*' ->
      if i = (n - 1) then
        raise (Decl_error (s', "undefined type."));
      Pointer (aux (i + 1))
    | _ ->
      begin
        match String.sub s i (n - i) with
          | "int" -> Tint
          | "bool" -> Tbool
          | "string" -> Tstring
          | _ ->
            if Smap.mem s s_env then
              (Tstruct s)
            else
              raise (Decl_error (s', "undefined type."));
      end
  in aux 0

(* Extracts the name of a structure *)
let id_of_type = function
  | s, e, Tstruct t -> s, e, t
  | _ -> assert false

(* If a list of types has only one element then returns it *)
let get_type = function
  | [t] -> t
  | l -> Tlist l

(* Checks if the type t is valid in v *)
let rec check_type v = function
  | sp, ep, Pointer t -> Pointer (check_type v (sp, ep, t))
  | _, _, t' as t->
    if List.mem t' v then
      t'
    else
      raise (Decl_error (id_of_type t, "undefined structure."))

(* Checks if the r_type l is only composed of types from v *)
let rec check_r_type v = function
  | [] -> []
  | t::q ->
    let q' = check_r_type v q in
    (check_type v t)::q'

(* Returns an Smap and a r_type representing the input types *)
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

(* Returns the list of raw_var from a list of var *)
let rec get_raw_var v =
  List.map (fun (l, (_, _, t)) -> (List.map (fun (_, _, x) -> x) l, t)) v

(* Strips the location from a list of localised elements *)
let rec strip_loc = function
  | [] -> []
  | (_, _, x)::q -> x::(strip_loc q)

(* Checks that a value of type t2 can be assigned to a value of type t1 *)
let compatible = function
  | Pointer _, Tnil
  | Tall, _ -> true
  | t1, t2 when t1 = t2 -> true
  | _ -> false

(* Returns the type of a constant *)
let type_cst = function
  | Cint x -> Tint, Cint x
  | Cstring s -> Tstring, Cstring s
  | Cbool b -> Tbool, Cbool b
  | Cnil -> Tnil, Cnil

(* Create the environment from the args *)
let create_env args =
  let env = Hashtbl.create 0 in
  Smap.iter (fun x t -> Hashtbl.add env (x, 0) (t, true, (d_pos, d_pos))) args;
  env

(* Add a variable with a level lev to the environment *)
let add_var (sp, ep, x' as x) t env lev =
  if x' <> "_" then
    begin
      if Hashtbl.mem env (x', lev) then
        raise (Decl_error (x, "this variable is already declared."));
      Hashtbl.add env (x', lev) (t, false, (sp, ep))
    end

(* Marks the var x as used in regards to the level *)
let rec use_var (_, _, x' as x) env = function
  | -1 -> raise (Decl_error (x, "undefined variable."))
  | l ->
    if Hashtbl.mem env (x', l) then
      begin
        let t, _, p = Hashtbl.find env (x', l) in
        Hashtbl.replace env (x', l) (t, true, p);
        t, l
      end
    else
      use_var x env (l - 1)

(* Type a left expression and raise an error if the expr is not a left value *)
let rec type_left_expr f_env s_env env lev = function
  | _, _, Evar "_" -> Tall, Tevar ("_", 0)
  | sp, ep, Evar x ->
    let t, l = use_var (sp, ep, x) env lev in
    t, Tevar (x, l)
  | _, _, Eattr(e, _) as e1 ->
    let _ = type_left_expr f_env s_env env lev e in
    type_expr f_env s_env env lev e1
  | _, _, Eunop(Uref, _) as e -> type_expr f_env s_env env lev e
  | e -> raise (Left_error e)

(* Type an expression *)
and type_expr f_env s_env env lev = function
  | _, _, Ecst x -> let t, c = type_cst x in t, Tecst x
  | sp, ep, Evar "_" ->
    raise (Decl_error ((sp, ep, "_"), "cannot use _ as a value"))
  | sp, ep, Evar x ->
    let t, l = use_var (sp, ep, x) env lev in
    t, Tevar (x, l)
  | _, _, Eattr(e, (_, _, i' as i)) ->
    begin
      let (t, _) as e' = type_expr f_env s_env env lev e in
      match t with
        | Tstruct s | Pointer(Tstruct s) ->
          begin
            let s' = snd (Smap.find s s_env) in
            try
              let t' = fst (Smap.find i' s') in
              (t', Teattr(e', i'))
            with
              | Not_found -> raise (Field_error (i, t))
          end
        | _ -> raise (Field_error (i, t))
    end
  | (_, _, Ebinop(b, e1, e2)) as e ->
    begin
      let (t1, _) as e1' = type_expr f_env s_env env lev e1 in
      let (t2, _) as e2' = type_expr f_env s_env env lev e2 in
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
              | Tnil, Pointer _ | Pointer _, Tnil ->
                (Tbool, Tebinop (b, e1', e2'))
              | _ when t1 = t2 ->
                (Tbool, Tebinop(b, e1', e2'))
              | _ -> raise (Typing_error (e1, [t1], [t2]));
          end
    end
  | _, _, Eunop(u, e) ->
    begin
      let (t, _) as e' = type_expr f_env s_env env lev e in
      match u, t with
        | Uneg, Tint -> Tint, Teunop(u, e')
        | Unot, Tbool -> Tbool, Teunop(u, e')
        | Uref, Pointer t' -> t', Teunop(u, e')
        | Uderef, t' when t' <> Tnil ->
          let _ = type_left_expr f_env s_env env lev e in
          Pointer t', Teunop(u, e')
        | _ -> raise (Typing_error (e, [t], [Tnil]))
    end
  | (sp, ep, Ecall(f, l)) as e ->
    begin
      let r, e' = type_call f_env s_env env lev (f, l) in
      match r with
        | [t] -> t, e'
        | _ ->
          raise (Decl_error
                   ((sp, ep, str_of_expr e),
                    "this function should return only one value"))
    end
  | sp, ep, Eprint _ as e ->
    let i = sp, ep, str_of_expr e in
    raise (Decl_error (i, "fmt.Print has no type and cannot be used here."))

(* Type_instr returns b1, b2, a with a the new AST, b1 if a value is returned
 * and b2 if fmt.Print was used *)
and type_instr f_env s_env env ret v fmt lev = function
  (* Instruction vide *)
  | _, _, Iempty ->
    false, false, Tiempty, 0
  (* Bloc d'instructions *)
  | _, _, Ibloc l ->
    let b = ref false and u = ref false and s = ref 0 in
    let l' = List.fold_left
               (fun l' i ->
                  let b', u', i', s' =
                    type_instr f_env s_env env ret v fmt (lev + 1) i in
                  b := !b || b';
                  u := !u || u';
                  s := !s +  s';
                  i'::l') [] l in
    (* On retire les variables des sous-blocs *)
    Hashtbl.iter
      (fun (x, l) (t, b, (sp, ep)) ->
         if l > lev then
           if b then
             Hashtbl.remove env (x, l)
           else
             raise (Decl_error((sp, ep, x), "variable declared but not used")))
      env;
    !b, !u, Tibloc l', !s
  (* Branche If *)
  | _, _, Iif(e, i1, i2) ->
    let (t, _) as e' = type_expr f_env s_env env lev e in
    if t <> Tbool then
      raise (Typing_error (e, [t], [Tbool]));
    let b1, u1, i1', s1 = type_instr f_env s_env env ret v fmt lev i1 in
    let b2, u2, i2', s2 = type_instr f_env s_env env ret v fmt lev i2 in
    b1 && b2, u1 || u2, Tiif(e', i1', i2'), (s1 + s2)
  (* Affichage du résultat d'un appel de fonction *)
  | _, _, Iexpr(sp, ep, Eprint [_, _, Ecall (f, l)]) ->
    let r, e = type_call f_env s_env env lev (f, l) in
    false, true, Tiprint [get_type r, e], 0
  (* Affichage d'une liste d'expressions *)
  | _, _, Iexpr(sp, ep, Eprint l as e) ->
    if not fmt then
      raise (Decl_error ((sp, ep, str_of_expr e), "fmt not imported."));
    false, true, Tiprint (List.map (type_expr f_env s_env env lev) l), 0
  (* Exécution d'une fonction *)
  | _, _, Iexpr(sp, ep, Ecall (f, l) as e) ->
    let r, e' = type_call f_env s_env env lev (f, l) in
    begin
      match e' with
        | Tenew t ->
          raise (Decl_error ((sp, ep, str_of_expr e),
                             "expression evaluated but not used."))
        | Tecall (f', l) ->
          false, false, Tiexec (get_type r, f', l), 0
        | Tecomp (f', r_e) ->
          false, false, Tiexec (get_type r, f', [r_e]), 0
        | _ -> assert false
    end
  (* Exécution d'une autre expression *)
  | sp, ep, Iexpr e ->
    raise (Decl_error ((sp, ep, str_of_expr e),
                       "expression evaluated but not used."))
  (*false, false, Tiexpr (type_expr f_env s_env env lev e), 0*)
  (* Boucle for *)
  | _, _, Ifor(e, i) ->
    let (t, _) as e' = type_expr f_env s_env env lev e in
    if t <> Tbool then
      raise (Typing_error (e, [t], [Tbool]));
    let _, u, i', s = type_instr f_env s_env env ret v fmt lev i in
    false, u, Tifor (e', i'), s
  (* Affectation du résultat d'une fonction *)
  | _, _, Iassoc(l1, [sp, ep, Ecall (f, l) as e]) ->
    let r, e' = type_call f_env s_env env lev (f, l) in
    if List.length l1 <> List.length r then
      raise (Decl_error ((sp, ep, str_of_expr e),
                         "this function is expected to return"^
                         (string_of_int (List.length l1)^"values.")));
    let l1' = List.fold_left2
                (fun l' e1 t ->
                   let (t', _) as e1' = type_left_expr f_env s_env env lev e1 in
                   if not (compatible (t', t)) then
                     raise (Typing_error (e1, [t'], [t]));
                   e1'::l') [] l1 r in
    false, false, Tiassoc(l1', [Tlist r, e']), 0
  (* Affectation des valeurs de l2 aux éléments de l1 *)
  | sp, ep, Iassoc(l1, l2) as i ->
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of = must have the same number of expressions."));
    let l1', l2' =
      List.fold_left2
        (fun (l1'', l2'') e1 e2 ->
           let (t1, _) as e1' = type_left_expr f_env s_env env lev e1 in
           let (t2, _) as e2' = type_expr f_env s_env env lev e2 in
           if not (compatible (t1, t2)) then
             raise (Typing_error (e2, [t2], [t2]));
           e1'::l1'', e2'::l2'') ([], []) l1 l2 in
    false, false, Tiassoc(l1', l2'), 0
  (* Déclaration de variables avec le résultat de fonctions *)
  | sp, ep, Ivar(l1, None, [_, _, Ecall (f, l)]) ->
    let r, e' = type_call f_env s_env env lev (f, l) in
    if List.length r <> List.length l1 then
      raise (Length_ident_error (List.length r, l1, sp, ep));
    List.iter2 (fun x t -> add_var x t env lev) l1 r;
    false, false, Tivar (strip_loc l1, None, [Tlist r, e']), List.length r
  (* Déclaration de variables avec des expressions *)
  | sp, ep, Ivar(l1, None, l2) as i ->
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of := must have the same number of expressions."));
    let l', s' = List.fold_left2
                   (fun (l, s) x (sp', ep', _ as e) ->
                      let (t, _) as e' = type_expr f_env s_env env lev e in
                      if t = Tnil then
                        raise (Decl_error ((sp', ep', str_of_expr e),
                                           "use of untyped nil."));
                      add_var x t env lev;
                      e'::l, s + 1 (*size s_env t*)) ([], 0) l1 l2 in
    false, false, Tivar (strip_loc l1, None, List.rev l'), s'
  (* Déclaration de variables typées avec un appel de fonction *)
  | sp, ep, Ivar(l1, Some t, [_, _, Ecall (f, l)]) ->
    let t' = check_type v t in
    let r, e' = type_call f_env s_env env lev (f, l) in
    if List.length r <> List.length l1 then
      raise (Length_ident_error (List.length r, l1, sp, ep));
    List.iter2
      (fun x t ->
         if not (compatible (t', t)) then
           raise (Decl_error (f, "this function is expected to return only "^
                                 "values of type "^(str_of_type t')));
         add_var x t env lev) l1 r;
    false, false, Tivar (strip_loc l1, None, [Tlist r, e']), List.length r
  (* Déclaration de variables typées *)
  | _, _, Ivar(l1, Some t, []) ->
    let t' = check_type v t in
    List.iter (fun x -> add_var x t' env lev) l1;
    false, false, Tivar(strip_loc l1, Some t', []), List.length l1
  (* Déclaration de variables typées avec des expressions *)
  | sp, ep, Ivar(l1, Some t, l2) as i ->
    let t = check_type v t in
    if List.length l1 <> List.length l2 then
      raise (Decl_error
               ((sp, ep, str_of_instr i),
                "both sides of := must have the same number of expressions."));
    let l' = List.fold_left2
               (fun l x e ->
                  let (t', _) as e' = type_expr f_env s_env env lev e in
                  if not (compatible (t, t')) then
                    raise (Typing_error (e, [t'], [t]));
                  add_var x t env lev;
                  e'::l) [] l1 l2 in
    false, false, Tivar(strip_loc l1, None, List.rev l'), List.length l1
  (* Renvoi des valeurs d'une fonction *)
  | _, _, Ireturn [_, _, Ecall (f, l) as e] ->
    let r, e' = type_call f_env s_env env lev (f, l) in
    if r <> ret then
      raise (Typing_error (e, r, ret));
    true, false, Tireturn [Tlist r, e'], 0
  (* Renvoi d'une liste d'expressions *)
  | sp, ep, Ireturn l as e_r ->
    if (List.length l <> List.length ret) then
      raise (Decl_error ((sp, ep, str_of_instr e_r),
                         (string_of_int (List.length ret))^
                         " values must be returned"));
    let l' = List.fold_left2
               (fun lis t1 e ->
                  let (t2, _) as e' = type_expr f_env s_env env lev e in
                  if not (compatible (t1, t2)) then
                    raise (Typing_error (e, [t2], [t1]));
                  e'::lis) [] ret l in
    true, false, Tireturn (List.rev l'), 0

(* Type_call returns the list of types returned and the transformed AST *)
and type_call f_env s_env env lev = function
  (* La fonction new est traitée spécialement *)
  | (_, _, "new"), [sp, ep, Evar s] ->
    let t = type_of_str (sp, ep, s) s_env in
    [Pointer t], Tenew t
  | (sp, ep, "new") as f, e ->
    let s = str_of_expr (sp, ep, Ecall(f, e)) in
    raise (Decl_error ((sp, ep, s), "new must be called on a structure."))
  (* Composition de fonctions *)
  | (_, _, f') as f, [_, _, Ecall(g, l) as e] ->
    if not (Smap.mem f' f_env) then
      raise (Decl_error (f, "undefined function."));
    let t, eg = type_call f_env s_env env lev (g, l) in
    let r, a, _ = Smap.find f' f_env in
    if a = [] then
      raise (Decl_error (g, "unexpected argument."));
    if List.length t <> List.length a then
      raise (Typing_error (e, t, r));
    List.iter2
      (fun t1 t2 -> if t1 <> t2 then raise (Typing_error (e, t, r)))
      t a;
    r, Tecomp (f', (Tlist t, eg))
  (* Cas de base des fonctions *)
  | (sp, ep, f') as f, l ->
    if not (Smap.mem f' f_env) then
      raise (Decl_error (f, "undefined function."));
    let r, a, _ = Smap.find f' f_env in
    if List.length l <> List.length a then
      raise (Length_expr_error (List.length a, l, sp, ep));
    let l' = List.fold_left2
               (fun lis t1 e ->
                  let t2, _ as e' = type_expr f_env s_env env lev e in
                  if not (compatible (t1, t2)) then
                    raise (Typing_error (e, [t2], [t1]));
                  e'::lis) [] a l in
    r, Tecall (f', (List.rev l'))

let check_file (fmt, l) =
  (* On ajoute toutes les structures dans la listes des types valides *)
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
  let add_env (f_m, s_m, p_m, v_m) = function
    | Dfunc(f, v, t, _) ->
      let _, _, f' = f in
      if Smap.mem f' f_m then
        raise (Decl_error (f, "This function"^a_decl));
      let r, vars = get_var_type v_types v in
      let t = check_r_type v_types t in
      Smap.add f' (t, r, vars) f_m, s_m, p_m, v_m
    | Dstruct(s, v) ->
      let sp, ep, s' = s in
      let _, vars = get_var_type v_types v in
      f_m, Smap.add s' (vars, v) s_m, Smap.add s' (sp, ep) p_m, Smap.add s' v v_m
  in
  let f_env, s_env, p_env, v_env =
    List.fold_left add_env (Smap.empty, Smap.empty, Smap.empty, Smap.empty) l in
  (* On vérifie qu'il n'y a pas de structures récursives *)
  let _ = check_recur s_env p_env in
  (* On calcule la taille des structures et les décalages des champs *)
  let s_env = get_sizes s_env in
  (* On vérifie que main est présente sans arguments ni valeur retournée *)
  if not (Smap.mem "main" f_env) then
    raise (Main_error "a main function must be declared.");
  let r, _, a = Smap.find "main" f_env in
  if r <> [] then
    raise (Main_error "main function cannot return anything.");
  if a <> Smap.empty then
    raise (Main_error "main function cannot take arguments.");
  (* Typage des instructions du programme *)
  let fmt' = ref false in
  let check_func l = function
    | Dfunc((_, _, f') as f, v, t, b) ->
      let r, t_a, env = Smap.find f' f_env in
      let env' = create_env env in
      let re, fu, b', s = type_instr f_env s_env env' r v_types fmt 0 b in
      fmt' := !fmt' || fu;
      if r != [] && not re then
        raise (Decl_error (f, "this function must return a value."));
      let (v':raw_var list) = get_raw_var v in
      (f', v', t, b', s)::l
    | Dstruct(s, v) -> l
  in
  let l' = List.fold_left check_func [] l in
  (* On vérifie que fmt est importé si besoin *)
  if fmt && (not !fmt') then
    raise (Import_error "fmt imported but not used.");
  fmt, l', s_env, v_env, f_env

