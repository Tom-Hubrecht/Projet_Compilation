
(* Routines pour afficher l'arbre de syntaxe abstraite *)

open Go_ast
open Format

let binop_tbl = Hashtbl.create 13
let () = List.iter (fun (s, t) -> Hashtbl.add binop_tbl s t)
           [Badd, " + "; Bsub, " - "; Bmul, " * "; Bdiv, "/"; Bmod, " % ";
            Band, " && "; Bor, " || "; Beq, " == "; Bneq, " != "; Blt, " < ";
            Ble, " <= "; Bgt, " > "; Bge, " >= "]

let p_str pp x =
  fprintf str_formatter "%a" pp x;
  flush_str_formatter ()

let p_ident fmt (_, _, s) = fprintf fmt "%s" s

let p_opt pp fmt = function
  | None -> ()
  | Some x -> fprintf fmt "%a" pp x

let rec p_list sep pp fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" pp x
  | x::q -> fprintf fmt ("%a%s%a") pp x sep (p_list sep pp) q

let rec p_type fmt = function
  | Tint -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Tstruct s -> fprintf fmt "%s" s
  | Pointer t -> fprintf fmt "*%a" p_type t
  | Tlist _ -> ()
  | Tnil -> ()
  | Tnoop -> ()

let p_r_type fmt = fprintf fmt "@[(%a)@]" (p_list ", " p_type)

let rec p_l_type fmt (_, _, t) = p_type fmt t

let p_l_r_type fmt = function
  | [] -> ()
  | [t] -> fprintf fmt "%a " p_l_type t
  | t -> fprintf fmt "@[(%a) @]" (p_list ", " p_l_type) t

let p_unop fmt = function
  | Unot -> fprintf fmt "!"
  | Uneg -> fprintf fmt "-"
  | Uref -> fprintf fmt "*"
  | Uderef -> fprintf fmt "&"

let p_binop fmt b = fprintf fmt "%s" (Hashtbl.find binop_tbl b)

let p_cst fmt = function
  | Cint s -> fprintf fmt "%s" s
  | Cstring s -> fprintf fmt "%S" s
  | Cbool b -> fprintf fmt "%B" b
  | Cnil -> fprintf fmt "nil"

let rec p_expr fmt = function
  | _, _, Ecst c -> p_cst fmt c
  | _, _, Evar v -> fprintf fmt "%s" v
  | _, _, Eattr(e, a) -> fprintf fmt "%a.%a" p_expr e p_ident a
  | _, _, Ecall(f, l) -> fprintf fmt "%a(%a)" p_ident f (p_list ", " p_expr) l
  | _, _, Eprint l -> fprintf fmt "@,fmt.Print(%a)" (p_list ", " p_expr) l
  | _, _, Eunop(u, e) -> fprintf fmt "%a%a" p_unop u p_expr e
  | _, _, Ebinop(b, e1, e2) ->
    fprintf fmt "%a%a%a" p_expr e1 p_binop b p_expr e2

let rec p_instr fmt (_, _, i) = match i with
  | Iempty -> ()
  | Ibloc b -> fprintf fmt "@,{@[<v 2>@,%a@]@,}" (p_list "" p_instr) b
  | Iif(e, i, (_, _, Iempty)) ->
    fprintf fmt "@[<v 2>if %a then@,%a@]" p_expr e p_instr i
  | Iif(e, i1, i2) ->
    fprintf fmt "@[<v 2>if %a then@,%a@,else@,%a@]" p_expr e p_instr i1 p_instr i2
  | Iexpr e -> fprintf fmt "%a;@," p_expr e
  | Iincr e -> fprintf fmt "@[<h 2>%a++@]" p_expr e
  | Idecr e -> fprintf fmt "@[<h 2>%a--@]" p_expr e
  | Ifor(e, i) -> fprintf fmt "@[<v 2>for %a@,{@,%a@,}@]" p_expr e p_instr i
  | Iassoc(l1, l2) ->
    fprintf fmt "@,@[%a = %a@];" (p_list ", " p_expr) l1 (p_list ", " p_expr) l2
  | Ivar(l1, None, l2) ->
    fprintf fmt "@,@[%a := %a@];"
      (p_list ", " p_ident) l1
      (p_list ", " p_expr) l2
  | Ivar(l1, Some t, l2) ->
    fprintf fmt "@,@[%a %a := %a@];"
      (p_list ", " p_ident) l1
      p_l_type t
      (p_list ", " p_expr) l2
  | Ireturn l -> fprintf fmt "@,@[return %a@];" (p_list ", " p_expr) l

let p_var fmt (l, t) = fprintf fmt "%a %a" (p_list ", " p_ident) l p_l_type t

let p_func fmt (f, l, t, i) =
  fprintf fmt "func %a(%a) %a@,{@[<v 2>%a@]@,}"
    p_ident f (p_list ", " p_var) l p_l_r_type t p_instr i

let p_struct fmt (s, l) =
  fprintf fmt "@[<hv 2>struct %a @,{@,%a@,}@]" p_ident s (p_list "@," p_var) l

let p_decl fmt = function
  | Dstruct s -> p_struct fmt s
  | Dfunc f -> p_func fmt f

let p_file fmt (b, l) = match b with
  | true -> fprintf fmt "import \"fmt\"@,@[<v>@,%a@]" (p_list "\n\n" p_decl) l
  | false -> fprintf fmt "%a" (p_list "\n\n" p_decl) l

let print_file = fprintf std_formatter "%a@." p_file
