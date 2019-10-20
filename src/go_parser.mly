
/* Analyseur syntaxique pour Petit Go */

%{
  open Go_ast

  exception Import_error of string
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT

%token ELSE FALSE FOR FUNC IF IMPORT NIL PACKAGE RETURN STRUCT TRUE TYPE VAR
%token FMT PRINT
%token INC "++"
%token DEC "--"
%token ALLOC "="
%token DEF ":="
%token AND "&&"
%token OR "||"
%token NOT "!"
%token EQ "=="
%token NEQ "!="
%token GT ">"
%token LT "<"
%token GTEQ ">="
%token LTEQ "<="
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token MOD "%"
%token AMP "&"
%token DOT "."
%token COMMA ","
%token SEMICOLON ";"
%token LPAR "("
%token LBRA "{"
%token RBRA "}"
%token RPAR ")"
%token EOF

/* Définition des priorités et de l'associativité */
%left "||"
%left "&&"
%left "==" "!=" ">" ">=" "<" "<="
%left "+" "-"
%left "*" "/" "%"
%nonassoc unary
%left "."

/* Point d'entrée de la grammaire */
%start file

/* Type renvoyé par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| PACKAGE id = IDENT imp = option(import) decls = decl* EOF
    {if id <> "main" then
       (raise Syntax_error)
     else
       (imp, decls)}
;

import:
| IMPORT s = STRING ";"
    {if s <> "fmt" then raise (Import_error "Unknown module") else s}

decl:
| s = struc
    {Dstruct s}
| f = fonc
    {Dfunc f}
;

struc:
| TYPE id = IDENT STRUCT "{" v = separated_list(";", vars) "}" ";"
    {id, v}
;

fonc:
| FUNC id = IDENT "(" v = separated_list(",", vars) ")" t = r_type b = bloc ";"
    {id, v, t, b}
;

vars:
| l = separated_nonempty_list(",", IDENT) t = v_type
    {l, t}
;

r_type:
| t = v_type;
    {[t]}
| "(" l = separated_nonempty_list(",", v_type) ")"
    {l}
;

v_type:
| id = IDENT
    {match id with
      | "int" -> Tint
      | "bool" -> Tbool
      | "string" -> Tstring
      | _ -> Tstruct id}
| "*" t = v_type
    {Pointer t}
;

expr:
| c = cst
    {Ecst c}
| "(" e = expr ")"
    {e}
| id = IDENT
    {Evar id}
| e = expr "." id = IDENT
    {Eattr(e, id)}
| id = IDENT "(" a = separated_list(",", expr) ")"
    {Ecall(id, a)}
| FMT "." PRINT "(" e = separated_list(",", expr) ")"
    //{if i1 <> "fmt" || i2 <> "Print" then raise Syntax_error else Eprint e}
    {Eprint e}
| PRINT
    {Evar "Print"}
| e = expr "." PRINT
    {Eattr(e, "Print")}
| PRINT "(" a = separated_list(",", expr) ")"
    {Ecall("Print", a)}
| u = unop e = expr %prec unary
    {Eunop(u, e)}
| e1 = expr o = op e2 = expr
    {Ebinop(o, e1, e2)}
;

cst:
| i = INT
    {Cint i}
| s = STRING
    {Cstring s}
| TRUE
    {Cbool true}
| FALSE
    {Cbool false}
| NIL
    {Cnil}
;

%inline unop:
| "!"
    {Unot}
| "-"
    {Uneg}
| "&"
    {Uderef}
| "*"
    {Uref}
;

%inline op:
| "=="
    {Beq}
| "!="
    {Bneq}
| "<"
    {Blt}
| "<="
    {Ble}
| ">"
    {Bgt}
| ">="
    {Bge}
| "+"
    {Badd}
| "-"
    {Bsub}
| "*"
    {Bmul}
| "/"
    {Bdiv}
| "%"
    {Bmod}
| "&&"
    {Band}
| "||"
    {Bor}
;

bloc:
| "{" i = separated_nonempty_list(";", instr) "}"
    {match i with
     | [x] -> x
     | _ -> Ibloc i}
;

instr:
| b = bloc
    {b}
| i = instr_s
    {i}
| i = instr_i
    {i}
| VAR v = separated_nonempty_list(",", IDENT) t = v_type?
    {Ivar(v, t, [])}
| VAR v = separated_nonempty_list(",", IDENT) t = v_type?
  "=" l = separated_nonempty_list(",", expr)
    {IVar(v, t, l)}
| RETURN e = separated_list(",", expr)
    {Ireturn e}
| FOR b = bloc
    {Ifor(Ecst(Cbool true), b)}
| FOR e = expr b = bloc
    {Ifor(e, b)}
;

instr_s:
| e = expr
    {Iexpr e}
| e = expr "++"
    {Iincr e}
| e = expr "--"
    {Idecr e}
| l = separated_nonempty_list(",", expr)
  "=" r = separated_nonempty_list(",", expr)
    {Iassoc(l, r)}
| l = separated_nonempty_list(",", IDENT) ":="
  r = separated_nonempty_list(",", expr)
    {Ivar(l, None, r)}
;

instr_i:
| IF e = expr b = bloc
    {Iif(e, b, Ibloc [])}
| IF e = expr b = bloc ELSE i = instr_i
    {Iif(e, b, i)}
| IF e = expr b1 = bloc ELSE b2 = bloc
    {Iif(e, b1, b2)}
;
