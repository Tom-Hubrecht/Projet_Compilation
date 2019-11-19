# 4 "go_lexer.mll"
 
open Lexing
open Go_parser

exception Lexing_error of string

let ins = ref false

let keywords = Hashtbl.create 13
let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t)
    ["else", ELSE; "false", FALSE; "for", FOR; "func", FUNC; "if", IF;
     "import", IMPORT; "nil", NIL; "package", PACKAGE; "return", RETURN;
     "struct", STRUCT; "true", TRUE; "type", TYPE; "var", VAR;]

let decimal s =
  try Int64.to_string (Int64.of_string (String.lowercase_ascii s)) with
  | Failure _ -> raise (Lexing_error "Range exceeded for int litteral")

# 21 "go_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\221\255\222\255\002\000\224\255\225\255\226\255\227\255\
    \228\255\229\255\230\255\231\255\233\255\003\000\030\000\031\000\
    \032\000\002\000\001\000\051\000\079\000\225\000\157\000\155\000\
    \048\001\095\000\254\255\003\000\252\255\253\255\058\001\096\001\
    \121\001\159\001\182\001\220\001\243\001\025\002\048\002\086\002\
    \109\002\147\002\170\002\208\002\231\002\013\003\036\003\247\255\
    \046\000\088\000\088\000\089\000\086\000\093\000\122\000\120\000\
    \118\000\117\000\122\000\161\000\160\000\161\000\164\000\162\000\
    \171\000\164\000\251\255\250\255\107\003\225\003\087\004\248\255\
    \246\255\245\255\244\255\243\255\242\255\241\255\223\255\217\000\
    \253\255\254\255\255\255\112\001\252\255\253\255\174\000\255\255\
    \254\255";
  Lexing.lex_backtrk =
   "\004\000\255\255\255\255\034\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\015\000\018\000\
    \017\000\034\000\019\000\020\000\006\000\034\000\021\000\004\000\
    \004\000\023\000\255\255\000\000\255\255\255\255\255\255\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\003\000\255\255\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\080\000\
    \000\000\000\000\000\000\084\000\000\000\000\000\255\255\000\000\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\026\000\000\000\027\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\015\000\021\000\027\000\000\000\011\000\018\000\072\000\
    \007\000\004\000\012\000\019\000\009\000\022\000\010\000\025\000\
    \024\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\003\000\008\000\014\000\016\000\013\000\078\000\
    \077\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\076\000\075\000\074\000\071\000\020\000\
    \049\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\006\000\017\000\005\000\073\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\028\000\050\000\051\000\052\000\053\000\029\000\054\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\055\000\056\000\057\000\058\000\020\000\059\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\047\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\060\000\048\000\061\000\
    \062\000\063\000\064\000\065\000\066\000\088\000\000\000\000\000\
    \000\000\000\000\000\000\082\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\068\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\068\000\068\000\067\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\069\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\032\000\031\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\087\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\086\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \030\000\033\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\081\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\035\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\036\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\000\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\037\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\000\000\000\000\000\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\038\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\000\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \039\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \085\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\040\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\041\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\042\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\043\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \044\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\000\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\045\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\000\000\000\000\
    \000\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\046\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\000\000\000\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\068\000\068\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\068\000\068\000\067\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\069\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\068\000\068\000\070\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\069\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\068\000\
    \068\000\067\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\069\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\027\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\027\000\255\255\000\000\000\000\018\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\014\000\015\000\016\000\019\000\000\000\
    \048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\017\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\025\000\049\000\050\000\051\000\052\000\025\000\053\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\054\000\055\000\056\000\057\000\020\000\058\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\022\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\059\000\022\000\060\000\
    \061\000\062\000\063\000\064\000\065\000\086\000\255\255\255\255\
    \255\255\255\255\255\255\079\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\021\000\021\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\083\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \024\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\083\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\031\000\031\000\031\000\031\000\031\000\031\000\255\255\
    \024\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\000\032\000\032\000\032\000\032\000\032\000\
    \255\255\031\000\031\000\031\000\031\000\031\000\031\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\079\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \033\000\033\000\033\000\033\000\033\000\033\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\255\255\034\000\
    \034\000\034\000\034\000\034\000\034\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\036\000\036\000\036\000\036\000\
    \036\000\036\000\255\255\255\255\255\255\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\255\255\036\000\036\000\036\000\036\000\
    \036\000\036\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \083\000\038\000\038\000\038\000\038\000\038\000\038\000\255\255\
    \255\255\255\255\037\000\037\000\037\000\037\000\037\000\037\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\040\000\040\000\
    \040\000\040\000\040\000\040\000\255\255\255\255\255\255\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\255\255\040\000\040\000\
    \040\000\040\000\040\000\040\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\042\000\042\000\042\000\042\000\042\000\
    \042\000\255\255\255\255\255\255\041\000\041\000\041\000\041\000\
    \041\000\041\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\255\255\042\000\042\000\042\000\042\000\042\000\
    \042\000\043\000\043\000\043\000\043\000\043\000\043\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \044\000\044\000\044\000\044\000\044\000\044\000\255\255\255\255\
    \255\255\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\255\255\
    \044\000\044\000\044\000\044\000\044\000\044\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\255\255\255\255\045\000\045\000\
    \045\000\045\000\045\000\045\000\068\000\068\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\046\000\046\000\046\000\
    \046\000\046\000\046\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\069\000\069\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \070\000\070\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 34 "go_lexer.mll"
                (token lexbuf)
# 429 "go_lexer.ml"

  | 1 ->
# 35 "go_lexer.mll"
         (new_line lexbuf;
          if !ins then
            begin
              ins := false;
              SEMICOLON
            end
          else
            token lexbuf)
# 441 "go_lexer.ml"

  | 2 ->
# 43 "go_lexer.mll"
         (l_comment lexbuf)
# 446 "go_lexer.ml"

  | 3 ->
# 44 "go_lexer.mll"
         (m_comment lexbuf)
# 451 "go_lexer.ml"

  | 4 ->
let
# 45 "go_lexer.mll"
               i
# 457 "go_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 45 "go_lexer.mll"
                 (ins := true; INT (decimal i))
# 461 "go_lexer.ml"

  | 5 ->
let
# 46 "go_lexer.mll"
           s
# 467 "go_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 46 "go_lexer.mll"
             (ins := true; STRING s)
# 471 "go_lexer.ml"

  | 6 ->
let
# 47 "go_lexer.mll"
             id
# 477 "go_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 47 "go_lexer.mll"
                (try
                   let t = Hashtbl.find keywords id in
                   ins := List.mem t [FALSE; NIL; RETURN; TRUE];
                   t
                 with Not_found -> ins := true; IDENT id)
# 485 "go_lexer.ml"

  | 7 ->
# 52 "go_lexer.mll"
         (ins := true; INC)
# 490 "go_lexer.ml"

  | 8 ->
# 53 "go_lexer.mll"
         (ins := true; DEC)
# 495 "go_lexer.ml"

  | 9 ->
# 54 "go_lexer.mll"
         (ins := false; AND)
# 500 "go_lexer.ml"

  | 10 ->
# 55 "go_lexer.mll"
         (ins := false; OR)
# 505 "go_lexer.ml"

  | 11 ->
# 56 "go_lexer.mll"
         (ins := false; EQ)
# 510 "go_lexer.ml"

  | 12 ->
# 57 "go_lexer.mll"
         (ins := false; NEQ)
# 515 "go_lexer.ml"

  | 13 ->
# 58 "go_lexer.mll"
         (ins := false; LTEQ)
# 520 "go_lexer.ml"

  | 14 ->
# 59 "go_lexer.mll"
         (ins := false; GTEQ)
# 525 "go_lexer.ml"

  | 15 ->
# 60 "go_lexer.mll"
        (ins := false; LT)
# 530 "go_lexer.ml"

  | 16 ->
# 61 "go_lexer.mll"
        (ins := false; GT)
# 535 "go_lexer.ml"

  | 17 ->
# 62 "go_lexer.mll"
        (ins := false; ALLOC)
# 540 "go_lexer.ml"

  | 18 ->
# 63 "go_lexer.mll"
        (ins := false; NOT)
# 545 "go_lexer.ml"

  | 19 ->
# 64 "go_lexer.mll"
        (ins := false; AMP)
# 550 "go_lexer.ml"

  | 20 ->
# 65 "go_lexer.mll"
        (ins := false; PLUS)
# 555 "go_lexer.ml"

  | 21 ->
# 66 "go_lexer.mll"
        (ins := false; MINUS)
# 560 "go_lexer.ml"

  | 22 ->
# 67 "go_lexer.mll"
        (ins := false; TIMES)
# 565 "go_lexer.ml"

  | 23 ->
# 68 "go_lexer.mll"
        (ins := false; DIV)
# 570 "go_lexer.ml"

  | 24 ->
# 69 "go_lexer.mll"
        (ins := false; MOD)
# 575 "go_lexer.ml"

  | 25 ->
# 70 "go_lexer.mll"
        (ins := false; DOT)
# 580 "go_lexer.ml"

  | 26 ->
# 71 "go_lexer.mll"
        (ins := false; COMMA)
# 585 "go_lexer.ml"

  | 27 ->
# 72 "go_lexer.mll"
        (ins := false; SEMICOLON)
# 590 "go_lexer.ml"

  | 28 ->
# 73 "go_lexer.mll"
        (ins := false; LPAR)
# 595 "go_lexer.ml"

  | 29 ->
# 74 "go_lexer.mll"
        (ins := false; LBRA)
# 600 "go_lexer.ml"

  | 30 ->
# 75 "go_lexer.mll"
        (ins := true; RBRA)
# 605 "go_lexer.ml"

  | 31 ->
# 76 "go_lexer.mll"
        (ins := true; RPAR)
# 610 "go_lexer.ml"

  | 32 ->
# 77 "go_lexer.mll"
         (ins := false; DEF)
# 615 "go_lexer.ml"

  | 33 ->
# 78 "go_lexer.mll"
        (ins := false; EOF)
# 620 "go_lexer.ml"

  | 34 ->
# 79 "go_lexer.mll"
      (raise (Lexing_error "Syntax error, illegal character"))
# 625 "go_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and l_comment lexbuf =
   __ocaml_lex_l_comment_rec lexbuf 79
and __ocaml_lex_l_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 82 "go_lexer.mll"
         (new_line lexbuf;
          if !ins then
            begin
              ins := false;
              SEMICOLON
            end
          else
            token lexbuf)
# 644 "go_lexer.ml"

  | 1 ->
# 90 "go_lexer.mll"
        (EOF)
# 649 "go_lexer.ml"

  | 2 ->
# 91 "go_lexer.mll"
      (l_comment lexbuf)
# 654 "go_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_l_comment_rec lexbuf __ocaml_lex_state

and m_comment lexbuf =
   __ocaml_lex_m_comment_rec lexbuf 83
and __ocaml_lex_m_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 94 "go_lexer.mll"
         (new_line lexbuf; m_comment lexbuf)
# 666 "go_lexer.ml"

  | 1 ->
# 95 "go_lexer.mll"
         (token lexbuf)
# 671 "go_lexer.ml"

  | 2 ->
# 96 "go_lexer.mll"
        (raise (Lexing_error "Unfinished comment"))
# 676 "go_lexer.ml"

  | 3 ->
# 97 "go_lexer.mll"
      (m_comment lexbuf)
# 681 "go_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_m_comment_rec lexbuf __ocaml_lex_state

;;
