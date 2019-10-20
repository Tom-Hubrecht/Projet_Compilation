
(* Analyseur lexical pour Petit Go *)

{
  open Lexing
  open Go_parser

  exception Lexing_error of string

  let keywords = Hashtbl.create 13
  let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t)
      ["else", ELSE; "false", FALSE; "for", FOR; "func", FUNC; "if", IF;
       "import", IMPORT; "nil", NIL; "package", PACKAGE; "return", RETURN;
       "struct", STRUCT; "true", TRUE; "type", TYPE; "var", VAR; "fmt", FMT;
       "Print", PRINT]

  let decimal s =
    let n = String.length s in
    if (n = 1) then
      (int_of_string s)
    else
      begin
        let value = function
          | '0' -> 0
          | '1' -> 1
          | '2' -> 2
          | '3' -> 3
          | '4' -> 4
          | '5' -> 5
          | '6' -> 6
          | '7' -> 7
          | '8' -> 8
          | '9' -> 9
          | 'a' | 'A' -> 10
          | 'b' | 'B' -> 11
          | 'c' | 'C' -> 12
          | 'd' | 'D' -> 13
          | 'e' | 'E' -> 14
          | 'f' | 'F' -> 16
          | _ -> assert false
        in
        if s.[1] = "x" || s.[1] = "X" then
          begin
            let i = ref 0 in
            String.iter
              (fun c -> i := 16*!i + (value c))
              (String.sub s 2 (n - 2));
            !i
          end
        else
          int_of_string s
      end
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit* | (("0x" | "0X") hexa+)
let car = [' ' '!' '#'-'[' ']'-'~' '\"' '\\' '\n' '\t' ]
let str = '"' car* '"'

rule token = parse
  | [' ' '\t']+ {token lexbuf}
  | '\n' {new_line lexbuf; token lexbuf}
  | "//" {l_comment lexbuf}
  | "/*" {m_comment lexbuf}
  | integer as i {INT (decimal i)}
  | str as s {STRING s}
  | ident as id {try Hashtbl.find keywords id with Not_found -> IDENT id}
  | "++" {INC}
  | "--" {DEC}
  | "&&" {AND}
  | "||" {OR}
  | "==" {EQ}
  | "!=" {NEQ}
  | "<=" {LTEQ}
  | ">=" {GTEQ}
  | "<" {LT}
  | ">" {GT}
  | "=" {ALLOC}
  | "!" {NOT}
  | "&" {AMP}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIV}
  | "%" {MOD}
  | "." {DOT}
  | "," {COMMA}
  | ";" {SEMICOLON}
  | "(" {LPAR}
  | "{" {LBRA}
  | "}" {RBRA}
  | ")" {RPAR}
  | ":=" {DEF}
  | eof {EOF}
  | _ {raise (Lexing_error "Syntax error, illegal character")}

and l_comment = parse
  | '\n' {new_line lexbuf; token lexbuf}
  | eof {EOF}
  | _ {s_comment lexbuf}

and m_comment = parse
  | '\n' {new_line lexbuf; m_comment lexbuf}
  | "*/" {token lexbuf}
  | eof {raise (Lexing_error "Unfinished comment")}
  | _ {m_comment lexbuf}


