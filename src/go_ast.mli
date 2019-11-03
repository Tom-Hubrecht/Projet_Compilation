
(* Arbre de syntaxe abstraite de Petit Go *)

type ident = string

type v_type =
  | Tint | Tbool | Tstring
  | Tstruct of string
  | Pointer of v_type

type r_type = v_type list

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

type expr =
  | Ecst of constant
  | Evar of ident
  | Eattr of expr * ident
  | Ecall of ident * expr list
  | Eprint of expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

type instr =
  | Iempty
  | Ibloc of instr list
  | Iif of expr * instr * instr
  | Iexpr of expr
  | Iincr of expr
  | Idecr of expr
  | Iassoc of expr list * expr list
  | Ivar of ident list * v_type option * expr list
  | Ireturn of expr list
  | Ifor of expr * instr

type var = ident list * v_type

type fonction = string * var list * r_type option * instr

type structure = string * var list

type decl =
  | Dstruct of structure
  | Dfunc of fonction

type file = string option * decl list

