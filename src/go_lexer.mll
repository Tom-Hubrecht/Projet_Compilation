
(* Analyseur lexical pour Petit Go *)

{
open Lexing
open Go_parser

exception Lexing_error of string

let ins = ref false
let neg = ref false

let keywords = Hashtbl.create 13
let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t)
    ["else", ELSE; "false", FALSE; "for", FOR; "func", FUNC; "if", IF;
     "import", IMPORT; "nil", NIL; "package", PACKAGE; "return", RETURN;
     "struct", STRUCT; "true", TRUE; "type", TYPE; "var", VAR;]

(* Transforms a 64bit stringed integer into a decimal *)
let decimal s =
  let s' = (if !neg then "-"^s else s) in
  try
    Int64.to_string (Int64.of_string (String.lowercase_ascii s'))
  with
  | Failure _ -> raise (Lexing_error "Range exceeded for int litteral.")

(* Only store the content of the string without the double quotes *)
let trim s =
  let n = String.length s in
  String.sub s 1 (n - 2)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit* | (("0x" | "0X") hexa+)
let e_c = "\\" "\""
let car = [' ' '!' '#'-'[' ']'-'~' '\\' '\n' '\t' ] | e_c
let str = '"' car* '"'

(* General lexer *)
rule token_raw = parse
  | [' ' '\t']+ {token_raw lexbuf}
  | '\n' {new_line lexbuf;
          if !ins then
              SEMICOLON
          else
            token_raw lexbuf}
  | "//" {l_comment lexbuf}
  | "/*" {m_comment lexbuf}
  | integer as i {INT (decimal i)}
  | str as s {STRING (trim s)}
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

(* Filters line comments *)
and l_comment = parse
  | '\n' {new_line lexbuf;
          if !ins then
              SEMICOLON
          else
            token_raw lexbuf}
  | eof {EOF}
  | _ {l_comment lexbuf}

(* Filters multiline comments *)
and m_comment = parse
  | '\n' {new_line lexbuf; m_comment lexbuf}
  | "*/" {token_raw lexbuf}
  | eof {raise (Lexing_error "Unfinished comment")}
  | _ {m_comment lexbuf}

{
let prec = ref EOF
let next = ref []

(* Do we need to insert a semicolon if we parse \n *)
let insert = function
  | IDENT _ | INT _ | STRING _
  | TRUE | FALSE | NIL | RETURN
  | INC | DEC
  | RPAR | RBRA -> true
  | _ -> false

(* Start to reduce a chain of - into only one if the the last token is
 * PLUS, TIMES, DIV, LPAR, LBRA, DEF, ALLOC, AND, OR, EQ, NEQ, LTEQ, GTEQ, LT,
 * GT*)
let track_neg () =
  List.mem !prec
    [AND; OR; EQ; NEQ; LTEQ; GTEQ; LT; GT; ALLOC; PLUS; TIMES; DIV; MOD;
     COMMA; SEMICOLON; LPAR; LBRA; DEF;]

(* Match the INT _ token *)
let is_int = function
  | INT i -> true
  | _ -> false

(* Wrapper around the lexer to check negative integers and insert semicolons *)
let token lexbuf =
  match !next with
    | [] ->
      begin
        let t = token_raw lexbuf in
        if t = MINUS then
          begin
            ins := false;
            (* Start to reduce the chained minuses if need be *)
            let seq = ref (track_neg ()) in
            let t' = ref MINUS in
            while !seq do
              neg := not !neg;
              t' := token_raw lexbuf;
              if !t' <> MINUS then
                  (* We recognized - T or T so we stop *)
                  seq := false;
            done;
            (* If we recogized T return T,
             * if we recognized - INT i return INT -i
             * otherwise store T and return MINUS *)
            if !neg then
              begin
                if is_int !t' then
                  begin
                    prec := !t';
                    ins := true;
                    neg := false;
                    !t'
                  end
                else
                  begin
                    prec := MINUS;
                    next := [!t'];
                    MINUS
                  end
              end
            else
              begin
                prec := !t';
                !t'
              end
          end
        else
          begin
            ins := insert t;
            neg := false;
            prec := t;
            t
          end
      end
    | t::q ->
      ins := insert t;
      prec := t;
      next := q;
      t
}
