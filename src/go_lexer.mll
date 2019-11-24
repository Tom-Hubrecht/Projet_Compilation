
(* Analyseur lexical pour Petit Go *)

{
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

let trim s =
  let n = String.length s in
  String.sub s 1 (n - 2)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let min_int = "-9223372036854775808" | (("0x" | "0X") "8000000000000000")
let integer = digit* | (("0x" | "0X") hexa+) | min_int
let e_c = "\\" '"'
let car = [' ' '!' '#'-'[' ']'-'~' '\\' '\n' '\t' ] | e_c
let str = '"' car* '"'

rule token = parse
  | [' ' '\t']+ {token lexbuf}
  | '\n' {new_line lexbuf;
          if !ins then
            begin
              ins := false;
              SEMICOLON
            end
          else
            token lexbuf}
  | "//" {l_comment lexbuf}
  | "/*" {m_comment lexbuf}
  | integer as i {ins := true; INT (decimal i)}
  | str as s {ins := true; STRING (trim s)}
  | ident as id {try
                   let t = Hashtbl.find keywords id in
                   ins := List.mem t [FALSE; NIL; RETURN; TRUE];
                   t
                 with Not_found -> ins := true; IDENT id}
  | "++" {ins := true; INC}
  | "--" {ins := true; DEC}
  | "&&" {ins := false; AND}
  | "||" {ins := false; OR}
  | "==" {ins := false; EQ}
  | "!=" {ins := false; NEQ}
  | "<=" {ins := false; LTEQ}
  | ">=" {ins := false; GTEQ}
  | "<" {ins := false; LT}
  | ">" {ins := false; GT}
  | "=" {ins := false; ALLOC}
  | "!" {ins := false; NOT}
  | "&" {ins := false; AMP}
  | "+" {ins := false; PLUS}
  | "-" {ins := false; MINUS}
  | "*" {ins := false; TIMES}
  | "/" {ins := false; DIV}
  | "%" {ins := false; MOD}
  | "." {ins := false; DOT}
  | "," {ins := false; COMMA}
  | ";" {ins := false; SEMICOLON}
  | "(" {ins := false; LPAR}
  | "{" {ins := false; LBRA}
  | "}" {ins := true; RBRA}
  | ")" {ins := true; RPAR}
  | ":=" {ins := false; DEF}
  | eof {ins := false; EOF}
  | _ {raise (Lexing_error "Syntax error, illegal character")}

and l_comment = parse
  | '\n' {new_line lexbuf;
          if !ins then
            begin
              ins := false;
              SEMICOLON
            end
          else
            token lexbuf}
  | eof {EOF}
  | _ {l_comment lexbuf}

and m_comment = parse
  | '\n' {new_line lexbuf; m_comment lexbuf}
  | "*/" {token lexbuf}
  | eof {raise (Lexing_error "Unfinished comment")}
  | _ {m_comment lexbuf}

