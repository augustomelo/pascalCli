{

    open Sintatico
    open Lexing
    open Printf

    let incr_num_linha lexbuf =
      let pos = lexbuf.lex_curr_p in
         lexbuf.lex_curr_p <- { pos with
          pos_lnum = pos.pos_lnum + 1;
          pos_bol = pos.pos_cnum;
    }

  let msg_erro lexbuf c =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    sprintf "%d-%d: caracter desconhecido %c" lin col c
}

let digito = ['0' - '9']
let int = '-'? digito+
let letra = [ 'a'-'z' 'A' - 'Z']
let identificador = letra ( letra | digito | '_' )*

rule token = parse
  [' ' '\t']         { token lexbuf }
| '\n'               { incr_num_linha lexbuf; token lexbuf }
| "program" 		{ PROGRAM }
| "function" 		{ FUNCAO }
| "return" 		{ RETURN }
|"begin" 		{ BEGIN }
|"end" 			{ END }
| '.' 			{PF}
| '('           	{ APAR }
| ')'           	{ FPAR }
| ','			{ VIRG }
| ':'           	{ DPTOS }
| ';'           	{ PTVIRG }
| ":=" 			{ ATRIB }
| int as num    	{ let numero = int_of_string num in
                         printf "INT %d\n" numero;
                         INT numero }

| identificador as id 	{ ID id }
| _ as c              	{ failwith (msg_erro lexbuf c); }
| eof                 	{ EOF }
