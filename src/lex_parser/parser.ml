type token =
  | INT of (int)
  | MINUS
  | STRING
  | ZERO
  | P
  | PC

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* MINUS *);
  259 (* STRING *);
  260 (* ZERO *);
  261 (* P *);
  262 (* PC *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\007\000\003\000\002\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\001\000\
\002\000"

let yydgoto = "\002\000\
\004\000\010\000\011\000"

let yysindex = "\007\000\
\252\254\000\000\006\255\000\000\009\255\010\255\005\255\000\000\
\011\255\007\255\001\255\000\000\012\255\005\255\005\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\255\013\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\001\000\245\255"

let yytablesize = 17
let yytable = "\015\000\
\003\000\008\000\009\000\015\000\014\000\008\000\009\000\001\000\
\005\000\006\000\007\000\012\000\013\000\003\000\017\000\016\000\
\004\000"

let yycheck = "\011\000\
\005\001\001\001\002\001\015\000\004\001\001\001\002\001\001\000\
\003\001\001\001\001\001\001\001\006\001\006\001\014\000\004\001\
\004\001"

let yynames_const = "\
  MINUS\000\
  STRING\000\
  ZERO\000\
  P\000\
  PC\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 14 "src/lex_parser/parser.mly"
                                                 ( (_3,_5) )
# 83 "src/lex_parser/parser.ml"
               : (int*Implementation.exp)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 17 "src/lex_parser/parser.mly"
                            ( Implementation.Et(_1,_3) )
# 91 "src/lex_parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'elem) in
    Obj.repr(
# 18 "src/lex_parser/parser.mly"
                            ( _1 )
# 98 "src/lex_parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'elem) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'elem) in
    Obj.repr(
# 21 "src/lex_parser/parser.mly"
                            ( Implementation.Ou(_1,_2) )
# 106 "src/lex_parser/parser.ml"
               : 'elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 22 "src/lex_parser/parser.mly"
                            ( Implementation.Var (_1-1) )
# 113 "src/lex_parser/parser.ml"
               : 'elem))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 23 "src/lex_parser/parser.mly"
                            ( Implementation.No (Implementation.Var (_2-1)) )
# 120 "src/lex_parser/parser.ml"
               : 'elem))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (int*Implementation.exp))
