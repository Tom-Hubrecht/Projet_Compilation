
(* Routines pour afficer l'arbre de syntaxe abstraite *)

open Go_ast
open Printf

let binop_tbl = Hashtbl.create 13
let () = List.iter (fun (s, t) -> Hashtbl.add binop_tbl s t)
    [Badd, " + "; Bsub, " - "; Bmul, " * "; Bdiv, "/"; Bmod, " % ";
     Band, " && "; Bor, " || "; Beq, " == "; Bneq, " != "; Blt, " < ";
     Ble, " <= "; Bgt, " > "; Bge, " >= "]

let rec print_type = function
  | Tint -> printf "int"
  | Tbool -> printf "bool"
  | Tstring -> printf "string"
  | Tstruct s -> printf "%s" s
  | Pointer t ->
    printf "*";
    print_type t

let print_r_type l =
  printf "(";
  List.iter (fun t -> print_type t; printf ", ") l;
  printf ")"

let print_unop = function
  | Unot -> printf " !"
  | Uneg -> printf " -"
  | Uref -> printf " *"
  | Uderef -> printf " &"

let print_binop b = printf "%s" (Hashtbl.find binop_tbl b)

let print_cst = function
  | Cint s -> printf "%s" s
  | Cstring s -> printf "%S" s
  | Cbool b -> printf "%B" b
  | Cnil -> printf "NULL"

let rec print_expr = function
  | Ecst c -> print_cst c
  | Evar v -> printf "%s" v
  | Eattr(e, a) ->
    print_expr e;
    printf ".%s" a
  | Ecall(f, l) ->
    printf "%s(" f;
    List.iter (fun e -> print_expr e; printf ", ";) l;
    printf ")"
  | Eprint l -> print_expr (Ecall("fmt.Print", l))
  | Eunop(u, e) ->
    print_unop u;
    print_expr e;
  | Ebinop(b, e1, e2) ->
    printf "(";
    print_expr e1;
    print_binop b;
    print_expr e2;
    printf ")"

let rec print_instr s = function
  | Iempty -> ()
  | Ibloc b ->
    printf "%s{\n" s;
    List.iter (print_instr (s^"  ")) b;
    printf "%s}\n" s
  | Iif(e, i, Iempty) ->
    printf "%sif " s;
    print_expr e;
    printf " then\n";
    print_instr (s^"  ") i;
  | Iif(e, i1, i2) ->
    printf "%sif " s;
    print_expr e;
    printf " then\n";
    print_instr (s^"  ") i1;
    printf "%selse\n" s;
    print_instr (s^"  ") i2
  | Iexpr e ->
    printf "%s" s;
    print_expr e;
    printf "\n"
  | Iincr e ->
    printf "%s" s;
    print_expr e;
    printf " ++\n"
  | Idecr e ->
    printf "%s" s;
    print_expr e;
    printf " --\n"
  | Ifor(e, i) ->
    printf "%sfor " s;
    print_expr e;
    printf "\n%s{\n" s;
    print_instr (s^"  ") i;
    printf "%s}\n" s
  | _ -> ()

let print_var (l, t) =
  let rec aux_var = function
    | [] -> ()
    | [v] -> printf "%s " v
    | v::q -> printf "%s, " v; aux_var q
  in
  aux_var l;
  print_type t

let print_func (f, l, t, i) =
  printf "%s(" f;
  List.iter (fun v -> print_var v; printf ",") l;
  printf ")";
  begin
    match t with
    | Some [v_t] -> print_type v_t
    | Some r_t -> print_r_type r_t
    | None -> ()
  end;
  printf "\n";
  print_instr "" i

let print_struct (s, l) =
  printf "%s\n{\n" s;
  List.iter (fun v -> printf "  "; print_var v; printf "\n";) l;
  printf "}\n"

let print_decl = function
  | Dstruct s ->
    printf "struct ";
    print_struct s
  | Dfunc f ->
    printf "func ";
    print_func f

let print_file (_, l) =
  List.iter (fun d -> print_decl d; print_newline ()) l
