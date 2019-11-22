
/* Analyseur syntaxique pour Petit Go */

%{
open Go_ast

exception Parsing_error of string
exception Syntax_error of Lexing.position * Lexing.position * string

let rec get_id = function
  | [] -> []
  | (s, e, Evar id)::q -> (s, e, id)::(get_id q)
  | _ -> raise Error

let rec filter = function
  | [] -> []
  | [_, _, Iempty] -> []
  | [x] -> [x]
  | (_, _, Iempty)::q -> filter q
  | x::q -> x::(filter q)

let content_of_loc (_, _, r_e) = r_e

let d_pos = Lexing.dummy_pos

%}

%token <string> INT
%token <string> STRING
%token <string> IDENT

%token ELSE FALSE FOR FUNC IF IMPORT NIL PACKAGE RETURN STRUCT TRUE TYPE VAR
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
%type <Go_ast.file> file

%%

sep_list(sep, X):
| x = X
    {[x]}
| x = X sep
    {[x]}
| x = X sep l = sep_list(sep, X)
    {x::l}
;

loc_ident:
| id = IDENT
    {$startpos, $endpos, id}
;

file:
| PACKAGE id = IDENT ";" decls = decl* EOF
    {if id <> "main" then
       raise Error
     else
       (false, decls)}
| PACKAGE id = IDENT ";" IMPORT s = STRING ";" decls = decl* EOF
    {if id <> "main" || s <> "\"fmt\"" then
       raise Error
     else
       (true, decls)}
;

decl:
| s = struc
    {Dstruct s}
| f = fonc
    {Dfunc f}
;

struc:
| TYPE id = loc_ident STRUCT "{" "}" ";"
    {id, []}
| TYPE id = loc_ident STRUCT "{" v = sep_list(";", vars) "}" ";"
    {id, v}
;

fonc:
| FUNC id = loc_ident "(" ")" t = r_type b = bloc ";"
    {id, [], t, b}
| FUNC id = loc_ident "(" ")" b = bloc ";"
    {id, [], [], b}
| FUNC id = loc_ident "(" v = sep_list(",", vars) ")" t = r_type b = bloc ";"
    {id, v, t, b}
| FUNC id = loc_ident "(" v = sep_list(",", vars) ")" b = bloc ";"
    {id, v, [], b}
;

vars:
| l = separated_nonempty_list(",", loc_ident) t = v_type
    {l, t}
;

r_type:
| t = v_type
    {[t]}
| "(" l = sep_list(",", v_type) ")"
    {l}
;

v_type:
| id = IDENT
    {match id with
      | "int" -> $startpos, $endpos, Tint
      | "bool" -> $startpos, $endpos, Tbool
      | "string" -> $startpos, $endpos, Tstring
      | _ -> $startpos, $endpos, Tstruct id}
| "*" t = v_type
    {let s, e, t' = t in s, e, Pointer t'}
;

expr:
| c = cst
    {$startpos, $endpos, Ecst c}
| "(" e = expr ")"
    {$startpos, $endpos, (content_of_loc e)}
| id = IDENT
    {$startpos, $endpos, Evar id}
| e = expr "." id = loc_ident
    {$startpos, $endpos, Eattr(e, id)}
| e = expr "." id = IDENT "(" l = separated_list(",", expr) ")"
    {if (content_of_loc e) = Evar "fmt" && id = "Print" then
       ($startpos, $endpos, Eprint l)
     else
       raise Error
    }
| id = loc_ident "(" a = separated_list(",", expr) ")"
    {$startpos, $endpos, Ecall(id, a)}
| u = unop e = expr %prec unary
    {$startpos, $endpos, Eunop(u, e)}
| e1 = expr o = op e2 = expr
    {$startpos, $endpos, Ebinop(o, e1, e2)}
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
| "{" i = separated_nonempty_list(";", instr)  "}"
    {match filter i with
     | [] -> $startpos, $endpos, Iempty
     | [_, _, x] -> $startpos, $endpos, x
     | _ -> $startpos, $endpos, Ibloc i}
;

instr:
| // Les instructions vides sont autorisées
    {d_pos, d_pos, Iempty}
| b = bloc
    {b}
| i = instr_s
    {i}
| i = instr_i
    {i}
| VAR v = separated_nonempty_list(",", loc_ident) t = v_type?
    {$startpos, $endpos, Ivar(v, t, [])}
| VAR v = separated_nonempty_list(",", loc_ident) t = v_type?
  "=" l = separated_nonempty_list(",", expr)
    {$startpos, $endpos, Ivar(v, t, l)}
| RETURN e = separated_list(",", expr)
    {$startpos, $endpos, Ireturn e}
| FOR b = bloc
    {$startpos, $endpos, Ifor((d_pos, d_pos, Ecst(Cbool true)), b)}
| FOR e = expr b = bloc
    {$startpos, $endpos, Ifor(e, b)}
| FOR ";" e = expr ";" b = bloc
    {$startpos, $endpos, Ifor(e, b)}
| FOR i = instr_s ";" e = expr ";" b = bloc
    {d_pos, d_pos, Ibloc [i;$startpos, $endpos, Ifor(e, b)]}
| FOR ";" e = expr ";" i = instr_s b = bloc
    {$startpos, $endpos, Ifor(e, (d_pos, d_pos, Ibloc [b; i]))}
| FOR i1 = instr_s ";" e = expr ";" i2 = instr_s b = bloc
    {d_pos, d_pos, Ibloc [i1; $startpos, $endpos,
                              Ifor(e, (d_pos, d_pos, Ibloc [b; i2]))]}
;

instr_s:
| e = expr
    {$startpos, $endpos, Iexpr e}
| e = expr "++"
    {$startpos, $endpos, Iincr e}
| e = expr "--"
    {$startpos, $endpos, Idecr e}
| l = separated_nonempty_list(",", expr) "="
  r = separated_nonempty_list(",", expr)
    {$startpos, $endpos, Iassoc(l, r)}
| l = separated_nonempty_list(",", expr) ":="
  r = separated_nonempty_list(",", expr)
    {$startpos, $endpos, Ivar((get_id l), None, r)}
;

instr_i:
| IF e = expr b = bloc
    {$startpos, $endpos, Iif(e, b, (d_pos, d_pos, Iempty))}
| IF e = expr b = bloc ELSE i = instr_i
    {$startpos, $endpos, Iif(e, b, i)}
| IF e = expr b1 = bloc ELSE b2 = bloc
    {$startpos, $endpos, Iif(e, b1, b2)}
;



