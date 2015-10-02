type token =
  | INT of (int)
  | ID of (string)
  | RETURN
  | FUNCAO
  | APAR
  | FPAR
  | DPTOS
  | VIRG
  | PTVIRG
  | PF
  | ATRIB
  | PROGRAM
  | BEGIN
  | END
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "sintatico.mly"
open Asabs;;
# 23 "sintatico.ml"
let yytransl_const = [|
  259 (* RETURN *);
  260 (* FUNCAO *);
  261 (* APAR *);
  262 (* FPAR *);
  263 (* DPTOS *);
  264 (* VIRG *);
  265 (* PTVIRG *);
  266 (* PF *);
  267 (* ATRIB *);
  268 (* PROGRAM *);
  269 (* BEGIN *);
  270 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\008\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\003\000\000\000\
\002\000"

let yydgoto = "\002\000\
\004\000\005\000\011\000\015\000"

let yysindex = "\255\255\
\245\254\000\000\000\255\000\000\003\000\251\254\000\000\248\254\
\003\255\252\254\255\254\008\255\253\254\000\000\000\000\002\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yytablesize = 12
let yytable = "\001\000\
\003\000\006\000\007\000\008\000\009\000\010\000\012\000\013\000\
\014\000\000\000\016\000\017\000"

let yycheck = "\001\000\
\012\001\002\001\000\000\009\001\013\001\003\001\011\001\009\001\
\001\001\255\255\014\001\010\001"

let yynames_const = "\
  RETURN\000\
  FUNCAO\000\
  APAR\000\
  FPAR\000\
  DPTOS\000\
  VIRG\000\
  PTVIRG\000\
  PF\000\
  ATRIB\000\
  PROGRAM\000\
  BEGIN\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'funcao) in
    Obj.repr(
# 17 "sintatico.mly"
                     ( _1 )
# 109 "sintatico.ml"
               : Asabs.decfn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'comandos) in
    Obj.repr(
# 23 "sintatico.mly"
 (
	{ fn_nome = _2;
	  (*fn_formais = $4;*)
	  fn_corpo = [_5];
	  fn_locais = []
	}
	)
# 123 "sintatico.ml"
               : 'funcao))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressao) in
    Obj.repr(
# 32 "sintatico.mly"
                                 ( Return _3 )
# 130 "sintatico.ml"
               : 'comandos))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 34 "sintatico.mly"
                ( Int _1 )
# 137 "sintatico.ml"
               : 'expressao))
(* Entry programa *)
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
let programa (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Asabs.decfn)
