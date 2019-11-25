
(* Arbre de syntaxe abstraite de Petit Go *)

type raw_ident = string

type ident = Lexing.position * Lexing.position * raw_ident

type raw_v_type =
  | Tint | Tbool | Tstring | Tnil | Tall
  | Tstruct of string
  | Pointer of raw_v_type
  | Tlist of raw_v_type list

type v_type = Lexing.position * Lexing.position * raw_v_type

type r_type = v_type list
type raw_r_type = raw_v_type list

type unop =
  | Unot
  | Uneg
  | Uref
  | Uderef

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Band | Bor
  | Beq | Bneq | Blt | Ble | Bgt | Bge

type constant =
  | Cint of string
  | Cstring of string
  | Cbool of bool
  | Cnil

type raw_expr =
  | Ecst of constant
  | Evar of raw_ident
  | Eattr of expr * ident
  | Ecall of ident * expr list
  | Eprint of expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
and expr = Lexing.position * Lexing.position * raw_expr

type r_expr =
  | Tecst of constant
  | Tevar of raw_ident
  | Teattr of t_expr * raw_ident
  | Tecall of raw_ident * r_expr list
  | Tecomp of raw_ident * r_expr
  | Tenew of raw_v_type
  | Teunop of unop * t_expr
  | Tebinop of binop * t_expr * t_expr
and t_expr = raw_v_type * r_expr

type r_instr =
  | Iempty
  | Ibloc of instr list
  | Iif of expr * instr * instr
  | Iexpr of expr
  | Iincr of expr
  | Idecr of expr
  | Ifor of expr * instr
  | Iassoc of expr list * expr list
  | Ivar of ident list * v_type option * expr list
  | Ireturn of expr list
and instr = Lexing.position * Lexing.position * r_instr

type t_instr =
  | Tiempty
  | Tibloc of t_instr list
  | Tiif of t_expr * t_instr * t_instr
  | Tiprint of t_expr list
  | Tiexpr of t_expr
  | Tiexec of r_expr
  | Tiincr of t_expr
  | Tidecr of t_expr
  | Tifor of t_expr * t_instr
  | Tiassoc of t_expr list * t_expr list
  | Tivar of ident list * v_type option * t_expr list
  | Tireturn of t_expr list

type var = ident list * v_type

type fonction = ident * var list * r_type * instr

type t_fonc = ident * var list * r_type * t_instr

type structure = ident * var list

type decl =
  | Dstruct of structure
  | Dfunc of fonction

type t_decl =
  | Tdstruct of structure
  | Tdfunc of t_fonc

type file = bool * decl list

type t_file = bool * t_decl list

