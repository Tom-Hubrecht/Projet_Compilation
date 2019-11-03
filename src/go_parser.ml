
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VAR
    | TYPE
    | TRUE
    | TIMES
    | STRUCT
    | STRING of (
# 21 "go_parser.mly"
       (string)
# 16 "go_parser.ml"
  )
    | SEMICOLON
    | RPAR
    | RETURN
    | RBRA
    | PLUS
    | PACKAGE
    | OR
    | NOT
    | NIL
    | NEQ
    | MOD
    | MINUS
    | LTEQ
    | LT
    | LPAR
    | LBRA
    | INT of (
# 20 "go_parser.mly"
       (string)
# 37 "go_parser.ml"
  )
    | INC
    | IMPORT
    | IF
    | IDENT of (
# 22 "go_parser.mly"
       (string)
# 45 "go_parser.ml"
  )
    | GTEQ
    | GT
    | FUNC
    | FOR
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DOT
    | DIV
    | DEF
    | DEC
    | COMMA
    | AND
    | AMP
    | ALLOC
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState158
  | MenhirState151
  | MenhirState150
  | MenhirState147
  | MenhirState141
  | MenhirState133
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState122
  | MenhirState120
  | MenhirState118
  | MenhirState116
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState97
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState61
  | MenhirState58
  | MenhirState57
  | MenhirState55
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState33
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState26
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState11
  | MenhirState7
  | MenhirState3

# 4 "go_parser.mly"
  
open Go_ast

exception Parsing_error of string

let rec get_id = function
  | [] -> []
  | (_, _, Evar id)::q -> id::(get_id q)
  | _ -> raise Error

let expr_content (_, _, r_e) = r_e

let d_pos = Lexing.dummy_pos


# 174 "go_parser.ml"

let rec _menhir_goto_instr_s : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instr_s -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv625 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv626)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv631 * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv627 * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv629 * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv633 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv634)
    | MenhirState141 | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv637 * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv635 * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : 'tv_instr_s)) = _menhir_stack in
        let _v : 'tv_instr = 
# 223 "go_parser.mly"
    (i)
# 258 "go_parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)) : 'freshtv638)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState103 | MenhirState55 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv599 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv597 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_separated_nonempty_list_COMMA_expr_)) = _menhir_stack in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 277 "go_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv603 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expr), _startpos_x_), _), _, (xs : 'tv_separated_nonempty_list_COMMA_expr_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 290 "go_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv607 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv605 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (v : 'tv_separated_nonempty_list_COMMA_IDENT_)), _, (t : 'tv_option_v_type_)), _, (l : 'tv_separated_nonempty_list_COMMA_expr_)) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : 'tv_instr = 
# 230 "go_parser.mly"
    (Ivar(v, t, l))
# 304 "go_parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
    | MenhirState141 | MenhirState40 | MenhirState129 | MenhirState111 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ALLOC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv609 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv610)
        | DEF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv611 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv613 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)) : 'freshtv616)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv619 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv617 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (l : 'tv_separated_nonempty_list_COMMA_expr_)), _, (r : 'tv_separated_nonempty_list_COMMA_expr_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_instr_s = 
# 259 "go_parser.mly"
    (Ivar((get_id l), None, r))
# 394 "go_parser.ml"
         in
        _menhir_goto_instr_s _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv623 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv621 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (l : 'tv_separated_nonempty_list_COMMA_expr_)), _, (r : 'tv_separated_nonempty_list_COMMA_expr_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_instr_s = 
# 256 "go_parser.mly"
    (Iassoc(l, r))
# 407 "go_parser.ml"
         in
        _menhir_goto_instr_s _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fonc : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fonc -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_fonc) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv593) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((f : 'tv_fonc) : 'tv_fonc) = _v in
    ((let _v : 'tv_decl = 
# 96 "go_parser.mly"
    (Dfunc f)
# 426 "go_parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv594)) : 'freshtv596)

and _menhir_goto_instr_i : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instr_i -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv587 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_instr_i) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv585 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((i : 'tv_instr_i) : 'tv_instr_i) = _v in
        ((let (((_menhir_stack, _menhir_s), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _, (b : 'tv_bloc)) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : 'tv_instr_i = 
# 266 "go_parser.mly"
    (Iif(e, b, i))
# 448 "go_parser.ml"
         in
        _menhir_goto_instr_i _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)) : 'freshtv588)
    | MenhirState141 | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_instr_i) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((i : 'tv_instr_i) : 'tv_instr_i) = _v in
        ((let _v : 'tv_instr = 
# 225 "go_parser.mly"
    (i)
# 463 "go_parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
    let _v : 'tv_instr_s = 
# 249 "go_parser.mly"
    (Iexpr e)
# 475 "go_parser.ml"
     in
    _menhir_goto_instr_s _menhir_env _menhir_stack _menhir_s _v

and _menhir_run123 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv583 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_instr_s = 
# 251 "go_parser.mly"
    (Iincr e)
# 490 "go_parser.ml"
     in
    _menhir_goto_instr_s _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)

and _menhir_run124 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv581 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_instr_s = 
# 253 "go_parser.mly"
    (Idecr e)
# 505 "go_parser.ml"
     in
    _menhir_goto_instr_s _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)

and _menhir_reduce78 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expr), _startpos_x_) = _menhir_stack in
    let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 515 "go_parser.ml"
     in
    _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run69 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run75 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run71 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run73 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run93 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run91 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run59 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv577 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 22 "go_parser.mly"
       (string)
# 994 "go_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv571 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1006 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv572)
        | ALLOC | AND | COMMA | DEC | DEF | DIV | DOT | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv573 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1046 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_), _), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 1051 "go_parser.ml"
            )), _startpos_id_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_id_ in
            let _v : 'tv_expr = let _endpos = _endpos_id_ in
            let _startpos = _startpos_e_ in
            
# 144 "go_parser.mly"
    (_startpos, _endpos, Eattr(e, id))
# 1061 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv574)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1071 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)) : 'freshtv578)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv579 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv580)

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv557 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1092 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv553 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1102 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv551 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1110 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos__6_ : Lexing.position) = _endpos in
            ((let (((((_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_), _), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 1116 "go_parser.ml"
            )), _startpos_id_), _startpos__4_), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos__6_ in
            let _v : 'tv_expr = let l = 
# 232 "<standard.mly>"
    ( xs )
# 1126 "go_parser.ml"
             in
            let _endpos = _endpos__6_ in
            let _startpos = _startpos_e_ in
            
# 146 "go_parser.mly"
    (if (expr_content e) = Evar "fmt" && id = "Print" then
       (_startpos, _endpos, Eprint l)
     else
       raise Error
    )
# 1137 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv552)) : 'freshtv554)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv555 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1147 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv565 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 1156 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 1166 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv559 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 1174 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 22 "go_parser.mly"
       (string)
# 1180 "go_parser.ml"
            )), _startpos_id_), _startpos__2_), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_id_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expr = let a = 
# 232 "<standard.mly>"
    ( xs )
# 1189 "go_parser.ml"
             in
            let _endpos = _endpos__4_ in
            let _startpos = _startpos_id_ in
            
# 152 "go_parser.mly"
    (_startpos, _endpos, Ecall(id, a))
# 1196 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv560)) : 'freshtv562)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv563 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 1206 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)) : 'freshtv566)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv569 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv567 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_instr = let e = 
# 232 "<standard.mly>"
    ( xs )
# 1220 "go_parser.ml"
         in
        
# 232 "go_parser.mly"
    (Ireturn e)
# 1225 "go_parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv568)) : 'freshtv570)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_instr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv541 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv539 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (i : 'tv_separated_nonempty_list_SEMICOLON_instr_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_bloc = 
# 214 "go_parser.mly"
    (if i = [Iempty] then Iempty else Ibloc i)
# 1253 "go_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv537) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bloc) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState106 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv487 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv481 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IF ->
                        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                    | LBRA ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv482)
                | RBRA | SEMICOLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv483 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _, (b : 'tv_bloc)) = _menhir_stack in
                    let _1 = () in
                    let _v : 'tv_instr_i = 
# 264 "go_parser.mly"
    (Iif(e, b, Iempty))
# 1289 "go_parser.ml"
                     in
                    _menhir_goto_instr_i _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv485 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
            | MenhirState108 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv491 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv489 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _, (b1 : 'tv_bloc)), _, (b2 : 'tv_bloc)) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_instr_i = 
# 268 "go_parser.mly"
    (Iif(e, b1, b2))
# 1310 "go_parser.ml"
                 in
                _menhir_goto_instr_i _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)) : 'freshtv492)
            | MenhirState120 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv495 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv493 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _menhir_s), _), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _), _, (i : 'tv_instr_s)), _, (b : 'tv_bloc)) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_instr = 
# 242 "go_parser.mly"
    (Ifor(e, Ibloc [b; i]))
# 1325 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
            | MenhirState114 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv499 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv497 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (((((_menhir_stack, _menhir_s), _), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _), _, (b : 'tv_bloc)) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_instr = 
# 238 "go_parser.mly"
    (Ifor(e, b))
# 1340 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
            | MenhirState130 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv503 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv501 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _menhir_s), _, (i1 : 'tv_instr_s)), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _), _, (i2 : 'tv_instr_s)), _, (b : 'tv_bloc)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_instr = 
# 244 "go_parser.mly"
    (Ibloc [i1; Ifor(e, Ibloc [b; i2])])
# 1355 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)) : 'freshtv504)
            | MenhirState129 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv507 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv505 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (((((_menhir_stack, _menhir_s), _, (i : 'tv_instr_s)), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _), _, (b : 'tv_bloc)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_instr = 
# 240 "go_parser.mly"
    (Ibloc [i; Ifor(e, b)])
# 1370 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
            | MenhirState133 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv511 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv509 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _, (b : 'tv_bloc)) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_instr = 
# 236 "go_parser.mly"
    (Ifor(e, b))
# 1383 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)) : 'freshtv512)
            | MenhirState111 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv515 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv513 * _menhir_state) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (b : 'tv_bloc)) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_instr = 
# 234 "go_parser.mly"
    (Ifor((d_pos, d_pos, Ecst(Cbool true)), b))
# 1396 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)) : 'freshtv516)
            | MenhirState40 | MenhirState141 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (b : 'tv_bloc)) = _menhir_stack in
                let _v : 'tv_instr = 
# 221 "go_parser.mly"
    (b)
# 1408 "go_parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)) : 'freshtv520)
            | MenhirState39 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv527 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1416 "go_parser.ml"
                ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMICOLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv523 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1426 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv521 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1433 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let ((((((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 1438 "go_parser.ml"
                    )), _startpos_id_), _startpos__3_), _endpos__4_, _), _, (t : 'tv_option_r_type_)), _, (b : 'tv_bloc)) = _menhir_stack in
                    let _7 = () in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_fonc = 
# 108 "go_parser.mly"
    (id, [], t, b)
# 1447 "go_parser.ml"
                     in
                    _menhir_goto_fonc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv522)) : 'freshtv524)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv525 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1457 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
            | MenhirState151 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv535 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1466 "go_parser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMICOLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((('freshtv531 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1476 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((('freshtv529 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1483 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (((((((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 1488 "go_parser.ml"
                    )), _startpos_id_), _startpos__3_), _, (v : 'tv_sep_list_COMMA_vars_)), _endpos__5_), _, (t : 'tv_option_r_type_)), _, (b : 'tv_bloc)) = _menhir_stack in
                    let _8 = () in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_fonc = 
# 110 "go_parser.mly"
    (id, v, t, b)
# 1497 "go_parser.ml"
                     in
                    _menhir_goto_fonc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((((('freshtv533 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 1507 "go_parser.ml"
                    ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
            | _ ->
                _menhir_fail ()) : 'freshtv538)) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv543 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_instr)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_instr)) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_instr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_instr)), _, (xs : 'tv_separated_nonempty_list_SEMICOLON_instr_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_instr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1530 "go_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_instr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)) : 'freshtv550)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expr -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv393 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv391 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_expr = let u = 
# 178 "go_parser.mly"
    (Uderef)
# 1558 "go_parser.ml"
             in
            let _startpos_u_ = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 154 "go_parser.mly"
    (_startpos, _endpos, Eunop(u, e))
# 1566 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv392)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv394)
    | MenhirState118 | MenhirState116 | MenhirState103 | MenhirState45 | MenhirState55 | MenhirState93 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | COMMA ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | DEF | LBRA | RBRA | RPAR | SEMICOLON ->
            _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv396)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _, _startpos__1_), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 201 "go_parser.mly"
    (Bmul)
# 1633 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1640 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv398)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv400)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 197 "go_parser.mly"
    (Badd)
# 1671 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1678 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv404)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 205 "go_parser.mly"
    (Bmod)
# 1703 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1710 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv408)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 203 "go_parser.mly"
    (Bdiv)
# 1735 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1742 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv412)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | COMMA | DEC | DEF | INC | LBRA | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 209 "go_parser.mly"
    (Bor)
# 1791 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1798 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv416)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 187 "go_parser.mly"
    (Bneq)
# 1833 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1840 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv420)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _, _startpos__1_), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 199 "go_parser.mly"
    (Bsub)
# 1871 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1878 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv422)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv424)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 191 "go_parser.mly"
    (Ble)
# 1913 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1920 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv428)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv429 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 189 "go_parser.mly"
    (Blt)
# 1955 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 1962 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv432)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 195 "go_parser.mly"
    (Bge)
# 1997 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 2004 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv436)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 193 "go_parser.mly"
    (Bgt)
# 2039 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 2046 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv440)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | NEQ | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 185 "go_parser.mly"
    (Beq)
# 2081 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 2088 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv442)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv444)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | AND | COMMA | DEC | DEF | INC | LBRA | OR | RBRA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr = let o = 
# 207 "go_parser.mly"
    (Band)
# 2135 "go_parser.ml"
             in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 156 "go_parser.mly"
    (_startpos, _endpos, Ebinop(o, e1, e2))
# 2142 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv446)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv448)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv451 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState97 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv449 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_expr = let _endpos = _endpos__3_ in
            let _startpos = _startpos__1_ in
            
# 140 "go_parser.mly"
    (_startpos, _endpos, (expr_content e))
# 2201 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv450)) : 'freshtv452)
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv454)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv455 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_expr = let u = 
# 176 "go_parser.mly"
    (Uneg)
# 2228 "go_parser.ml"
             in
            let _startpos_u_ = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 154 "go_parser.mly"
    (_startpos, _endpos, Eunop(u, e))
# 2236 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv458)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_expr = let u = 
# 174 "go_parser.mly"
    (Unot)
# 2261 "go_parser.ml"
             in
            let _startpos_u_ = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 154 "go_parser.mly"
    (_startpos, _endpos, Eunop(u, e))
# 2269 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv462)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ALLOC | AND | COMMA | DEC | DEF | DIV | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_expr = let u = 
# 180 "go_parser.mly"
    (Uref)
# 2294 "go_parser.ml"
             in
            let _startpos_u_ = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 154 "go_parser.mly"
    (_startpos, _endpos, Eunop(u, e))
# 2302 "go_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv464)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv466)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv468)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv471 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState113 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRA ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv470)
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv472)
    | MenhirState40 | MenhirState141 | MenhirState129 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | COMMA ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DEC ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | INC ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | DEF ->
            _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack)
        | LBRA | RBRA | SEMICOLON ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv474)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv477 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv475 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState128 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRA ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv476)
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv478)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | COMMA ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DEC ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DIV ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | EQ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | INC ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LTEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MINUS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TIMES ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | DEF ->
            _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv480)
    | _ ->
        _menhir_fail ()

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 2603 "go_parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_cst : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_cst -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv389) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_cst) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_cst) : 'tv_cst) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_expr = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 138 "go_parser.mly"
    (_startpos, _endpos, Ecst c)
# 2628 "go_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv388)) : 'freshtv390)

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_instr = 
# 219 "go_parser.mly"
    (Iempty)
# 2637 "go_parser.ml"
     in
    _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRA | SEMICOLON ->
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRA ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState111 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112) : 'freshtv386)
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_goto_instr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | STRING _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | RBRA | SEMICOLON ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv378)
    | RBRA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_instr)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_instr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2850 "go_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_instr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_cst = 
# 165 "go_parser.mly"
    (Cbool true)
# 2875 "go_parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv376)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 21 "go_parser.mly"
       (string)
# 2915 "go_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 21 "go_parser.mly"
       (string)
# 2926 "go_parser.ml"
    )) : (
# 21 "go_parser.mly"
       (string)
# 2930 "go_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_cst = 
# 163 "go_parser.mly"
    (Cstring s)
# 2938 "go_parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv374)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv371) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_cst = 
# 169 "go_parser.mly"
    (Cnil)
# 2989 "go_parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv372)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run53 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 20 "go_parser.mly"
       (string)
# 3062 "go_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv369) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 20 "go_parser.mly"
       (string)
# 3073 "go_parser.ml"
    )) : (
# 20 "go_parser.mly"
       (string)
# 3077 "go_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_cst = 
# 161 "go_parser.mly"
    (Cint i)
# 3085 "go_parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv370)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 22 "go_parser.mly"
       (string)
# 3092 "go_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 3104 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv364)
    | ALLOC | AND | COMMA | DEC | DEF | DIV | DOT | EQ | GT | GTEQ | INC | LBRA | LT | LTEQ | MINUS | MOD | NEQ | OR | PLUS | RBRA | RPAR | SEMICOLON | TIMES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 3144 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 22 "go_parser.mly"
       (string)
# 3149 "go_parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_id_ in
        let _v : 'tv_expr = let _endpos = _endpos_id_ in
        let _startpos = _startpos_id_ in
        
# 142 "go_parser.mly"
    (_startpos, _endpos, Evar id)
# 3158 "go_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv366)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 3168 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_cst = 
# 167 "go_parser.mly"
    (Cbool false)
# 3187 "go_parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv362)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRA ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | RBRA | SEMICOLON ->
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_goto_r_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_r_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_r_type) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : 'tv_r_type) : 'tv_r_type) = _v in
    ((let _v : 'tv_option_r_type_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3282 "go_parser.ml"
     in
    _menhir_goto_option_r_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)

and _menhir_goto_sep_list_COMMA_v_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sep_list_COMMA_v_type_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv347 * _menhir_state * 'tv_v_type)) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv345 * _menhir_state * 'tv_v_type)) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_v_type)), _, (l : 'tv_sep_list_COMMA_v_type_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_sep_list_COMMA_v_type_ = 
# 76 "go_parser.mly"
    (x::l)
# 3300 "go_parser.ml"
         in
        _menhir_goto_sep_list_COMMA_v_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * _menhir_state * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : 'tv_sep_list_COMMA_v_type_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_r_type = 
# 122 "go_parser.mly"
    (l)
# 3323 "go_parser.ml"
             in
            _menhir_goto_r_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv353 * _menhir_state * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_v_type_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sep_list_COMMA_vars_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sep_list_COMMA_vars_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state * 'tv_vars)) * _menhir_state * 'tv_sep_list_COMMA_vars_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv335 * _menhir_state * 'tv_vars)) * _menhir_state * 'tv_sep_list_COMMA_vars_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_vars)), _, (l : 'tv_sep_list_COMMA_vars_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_sep_list_COMMA_vars_ = 
# 76 "go_parser.mly"
    (x::l)
# 3350 "go_parser.ml"
         in
        _menhir_goto_sep_list_COMMA_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv343 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3358 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv339 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3368 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRA ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv341 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3394 "go_parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sep_list_SEMICOLON_vars_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sep_list_SEMICOLON_vars_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * 'tv_vars)) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state * 'tv_vars)) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_vars)), _, (l : 'tv_sep_list_SEMICOLON_vars_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_sep_list_SEMICOLON_vars_ = 
# 76 "go_parser.mly"
    (x::l)
# 3415 "go_parser.ml"
         in
        _menhir_goto_sep_list_SEMICOLON_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv333 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3423 "go_parser.ml"
        ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv329 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3433 "go_parser.ml"
            ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv325 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3443 "go_parser.ml"
                ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv323 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3450 "go_parser.ml"
                ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 3455 "go_parser.ml"
                )), _startpos_id_), _, (v : 'tv_sep_list_SEMICOLON_vars_)) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_struc = 
# 103 "go_parser.mly"
    (id, v)
# 3465 "go_parser.ml"
                 in
                _menhir_goto_struc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)) : 'freshtv326)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv327 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3475 "go_parser.ml"
                ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv331 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3486 "go_parser.ml"
            ) * Lexing.position))) * _menhir_state * 'tv_sep_list_SEMICOLON_vars_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_v_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_v_type_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv317 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALLOC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv312)
    | RBRA | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv313 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (v : 'tv_separated_nonempty_list_COMMA_IDENT_)), _, (t : 'tv_option_v_type_)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_instr = 
# 227 "go_parser.mly"
    (Ivar(v, t, []))
# 3541 "go_parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_file : _menhir_env -> 'ttv_tail -> (
# 66 "go_parser.mly"
      (Go_ast.file)
# 3560 "go_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
    let (_v : (
# 66 "go_parser.mly"
      (Go_ast.file)
# 3568 "go_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
    let ((_1 : (
# 66 "go_parser.mly"
      (Go_ast.file)
# 3575 "go_parser.ml"
    )) : (
# 66 "go_parser.mly"
      (Go_ast.file)
# 3579 "go_parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv308)) : 'freshtv310)

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNC ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | TYPE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | EOF ->
        _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158) : 'freshtv306)

and _menhir_goto_option_r_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_r_type_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv301 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3611 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv302)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv303 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3627 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRA ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv304)
    | _ ->
        _menhir_fail ()

and _menhir_goto_v_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_v_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state * Lexing.position) * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * Lexing.position) * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (t : 'tv_v_type)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_v_type = 
# 133 "go_parser.mly"
    (Pointer t)
# 3655 "go_parser.ml"
         in
        _menhir_goto_v_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)) : 'freshtv256)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (l : 'tv_separated_nonempty_list_COMMA_IDENT_)), _, (t : 'tv_v_type)) = _menhir_stack in
        let _v : 'tv_vars = 
# 115 "go_parser.mly"
    (l, t)
# 3667 "go_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_vars) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState14 | MenhirState7 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRA ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_vars)) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (x : 'tv_vars)) = _menhir_stack in
                    let _2 = () in
                    let _v : 'tv_sep_list_SEMICOLON_vars_ = 
# 74 "go_parser.mly"
    ([x])
# 3697 "go_parser.ml"
                     in
                    _menhir_goto_sep_list_SEMICOLON_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv260)
            | RBRA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_vars)) = _menhir_stack in
                let _v : 'tv_sep_list_SEMICOLON_vars_ = 
# 72 "go_parser.mly"
    ([x])
# 3711 "go_parser.ml"
                 in
                _menhir_goto_sep_list_SEMICOLON_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
        | MenhirState147 | MenhirState29 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_vars)) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (x : 'tv_vars)) = _menhir_stack in
                    let _2 = () in
                    let _v : 'tv_sep_list_COMMA_vars_ = 
# 74 "go_parser.mly"
    ([x])
# 3743 "go_parser.ml"
                     in
                    _menhir_goto_sep_list_COMMA_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv270)
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_vars)) = _menhir_stack in
                let _v : 'tv_sep_list_COMMA_vars_ = 
# 72 "go_parser.mly"
    ([x])
# 3757 "go_parser.ml"
                 in
                _menhir_goto_sep_list_COMMA_vars_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_vars) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
        | _ ->
            _menhir_fail ()) : 'freshtv278)) : 'freshtv280)) : 'freshtv282)
    | MenhirState33 | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TIMES ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv283 * _menhir_state * 'tv_v_type)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_v_type)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_sep_list_COMMA_v_type_ = 
# 74 "go_parser.mly"
    ([x])
# 3793 "go_parser.ml"
                 in
                _menhir_goto_sep_list_COMMA_v_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv286)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_v_type)) = _menhir_stack in
            let _v : 'tv_sep_list_COMMA_v_type_ = 
# 72 "go_parser.mly"
    ([x])
# 3807 "go_parser.ml"
             in
            _menhir_goto_sep_list_COMMA_v_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | MenhirState150 | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (t : 'tv_v_type)) = _menhir_stack in
        let _v : 'tv_r_type = 
# 120 "go_parser.mly"
    ([t])
# 3826 "go_parser.ml"
         in
        _menhir_goto_r_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_v_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_v_type)) = _menhir_stack in
        let _v : 'tv_option_v_type_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3838 "go_parser.ml"
         in
        _menhir_goto_option_v_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_IDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_IDENT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 3853 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 3859 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 22 "go_parser.mly"
       (string)
# 3864 "go_parser.ml"
        )), _startpos_x_), _, (xs : 'tv_separated_nonempty_list_COMMA_IDENT_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_IDENT_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3870 "go_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
    | MenhirState29 | MenhirState147 | MenhirState7 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv248)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ALLOC | RBRA | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState42 in
            ((let _v : 'tv_option_v_type_ = 
# 114 "<standard.mly>"
    ( None )
# 3904 "go_parser.ml"
             in
            _menhir_goto_option_v_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv252)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv229) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3923 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 3927 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv225) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3937 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 3941 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv223) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3947 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 3951 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 3956 "go_parser.ml"
            )), _startpos_id_), _), _endpos_s_, (s : (
# 21 "go_parser.mly"
       (string)
# 3960 "go_parser.ml"
            )), _startpos_s_), _, (decls : 'tv_list_decl_)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 66 "go_parser.mly"
      (Go_ast.file)
# 3970 "go_parser.ml"
            ) = 
# 86 "go_parser.mly"
    (if id <> "main" || s <> "\"fmt\"" then
       raise Error
     else
       (true, decls))
# 3977 "go_parser.ml"
             in
            _menhir_goto_file _menhir_env _menhir_stack _v) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv227) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 3987 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 3991 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_decl)), _, (xs : 'tv_list_decl_)) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 4004 "go_parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv241) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4012 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv237) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4022 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv235) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4028 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 4033 "go_parser.ml"
            )), _startpos_id_), _, (decls : 'tv_list_decl_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 66 "go_parser.mly"
      (Go_ast.file)
# 4041 "go_parser.ml"
            ) = 
# 81 "go_parser.mly"
    (if id <> "main" then
       raise Error
     else
       (false, decls))
# 4048 "go_parser.ml"
             in
            _menhir_goto_file _menhir_env _menhir_stack _v) : 'freshtv236)) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv239) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4058 "go_parser.ml"
            ) * Lexing.position)) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | _ ->
        _menhir_fail ()

and _menhir_goto_struc : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_struc -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv221) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_struc) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv219) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : 'tv_struc) : 'tv_struc) = _v in
    ((let _v : 'tv_decl = 
# 94 "go_parser.mly"
    (Dstruct s)
# 4078 "go_parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_r_type_ = 
# 114 "<standard.mly>"
    ( None )
# 4087 "go_parser.ml"
     in
    _menhir_goto_option_r_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TIMES ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run17 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 22 "go_parser.mly"
       (string)
# 4124 "go_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
    let (_endpos_id_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((id : (
# 22 "go_parser.mly"
       (string)
# 4135 "go_parser.ml"
    )) : (
# 22 "go_parser.mly"
       (string)
# 4139 "go_parser.ml"
    )) = _v in
    let (_startpos_id_ : Lexing.position) = _startpos in
    ((let _v : 'tv_v_type = 
# 127 "go_parser.mly"
    (match id with
      | "int" -> Tint
      | "bool" -> Tbool
      | "string" -> Tstring
      | _ -> Tstruct id)
# 4149 "go_parser.ml"
     in
    _menhir_goto_v_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 22 "go_parser.mly"
       (string)
# 4156 "go_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 4168 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv212)
    | ALLOC | IDENT _ | RBRA | SEMICOLON | TIMES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 4184 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 22 "go_parser.mly"
       (string)
# 4189 "go_parser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_IDENT_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4194 "go_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 4204 "go_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv57 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4222 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) * _menhir_state * 'tv_option_r_type_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv59 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4231 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_sep_list_COMMA_vars_) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_vars)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_instr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv67 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv69 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv71 * _menhir_state) * _menhir_state * 'tv_instr_s)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv73 * _menhir_state) * _menhir_state * 'tv_instr_s)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv77 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * _menhir_state * 'tv_instr_s) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_separated_nonempty_list_COMMA_expr_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv83 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv91 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv129 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv133 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv163 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4495 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 4514 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv179 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) * _menhir_state * 'tv_option_v_type_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv187 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4563 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_option_r_type_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_v_type)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv193 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4582 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4591 "go_parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv197) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4600 "go_parser.ml"
        ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 4604 "go_parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENT_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state * 'tv_vars)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * Lexing.position * _menhir_state * (
# 22 "go_parser.mly"
       (string)
# 4628 "go_parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv207 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4637 "go_parser.ml"
        ) * Lexing.position))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4646 "go_parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv210)

and _menhir_reduce57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 211 "<standard.mly>"
    ( [] )
# 4655 "go_parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 22 "go_parser.mly"
       (string)
# 4672 "go_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRUCT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv47 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4684 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv43 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4694 "go_parser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRA ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv41 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4706 "go_parser.ml"
                    ) * Lexing.position))) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState7 in
                    ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | SEMICOLON ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv37 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4718 "go_parser.ml"
                        ) * Lexing.position))) * _menhir_state) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv35 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4725 "go_parser.ml"
                        ) * Lexing.position))) * _menhir_state) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 22 "go_parser.mly"
       (string)
# 4730 "go_parser.ml"
                        )), _startpos_id_), _) = _menhir_stack in
                        let _6 = () in
                        let _5 = () in
                        let _4 = () in
                        let _3 = () in
                        let _1 = () in
                        let _v : 'tv_struc = 
# 101 "go_parser.mly"
    (id, [])
# 4740 "go_parser.ml"
                         in
                        _menhir_goto_struc _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)) : 'freshtv38)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv39 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4750 "go_parser.ml"
                        ) * Lexing.position))) * _menhir_state) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv44)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv45 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4765 "go_parser.ml"
                ) * Lexing.position)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv49 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4776 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 22 "go_parser.mly"
       (string)
# 4801 "go_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv27 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4813 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv25 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4827 "go_parser.ml"
                ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_menhir_s : _menhir_state) = MenhirState29 in
                ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAR ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TIMES ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LBRA ->
                    _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv26)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4858 "go_parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 66 "go_parser.mly"
      (Go_ast.file)
# 4885 "go_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv23) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PACKAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 22 "go_parser.mly"
       (string)
# 4917 "go_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv11) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4929 "go_parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FUNC ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState3
                | IMPORT ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv9) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4941 "go_parser.ml"
                    ) * Lexing.position)) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState3 in
                    ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | STRING _v ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ((('freshtv5) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4953 "go_parser.ml"
                        ) * Lexing.position)) * _menhir_state) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let (_v : (
# 21 "go_parser.mly"
       (string)
# 4959 "go_parser.ml"
                        )) = _v in
                        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | SEMICOLON ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : (((('freshtv1) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4971 "go_parser.ml"
                            ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 4975 "go_parser.ml"
                            ) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | FUNC ->
                                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                            | TYPE ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                            | EOF ->
                                _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv2)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : (((('freshtv3) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 4997 "go_parser.ml"
                            ) * Lexing.position)) * _menhir_state) * Lexing.position * (
# 21 "go_parser.mly"
       (string)
# 5001 "go_parser.ml"
                            ) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)) : 'freshtv6)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ((('freshtv7) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 5012 "go_parser.ml"
                        ) * Lexing.position)) * _menhir_state) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
                | TYPE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
                | EOF ->
                    _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState3
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv12)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv13) * Lexing.position * (
# 22 "go_parser.mly"
       (string)
# 5031 "go_parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv14)) : 'freshtv16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv18)) : 'freshtv20)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv22)) : 'freshtv24))

# 269 "<standard.mly>"
  

# 5050 "go_parser.ml"
