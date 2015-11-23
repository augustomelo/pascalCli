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
let digitofloat = digito+ ['.'] digito+
let letra = [ 'a'-'z' 'A' - 'Z']
let identificador = letra ( letra | digito | '_' )*
let booleano = "true" | "false"
let caracter = (digito | letra)

rule token = parse
  [' ' '\t']                 { token lexbuf }
  | '\n'                     { incr_num_linha lexbuf; token lexbuf }
  | "//"[ ^ '\n' ]*          { token lexbuf } (* ignorar comentario de linha *)
  | "/*"                     { comentario 0 lexbuf; }
  | "program"                { PROGRAM }
  | "function"               { FUNCAO }
  | "return"                 { RETURN }
  | "begin"                  { BEGIN }
  | "end"                    { END }
  | ':'                      { PTPT}
  | '.'                      { PF}
  | '('                      { APAR }
  | ')'                      { FPAR }
  | ','                      { VIRG }
  | ';'                      { PTVIRG }
  | '+'                      { MAIS }
  | '*'                      { VEZES }
  | '-'                      { MENOS }
  | '/'                      { DIV }
  | '>'                      { MAIOR }
  | '<'                      { MENOR }
  | ":="                     { ATRIB }
  | ">="                     { MAIORIGUAL }
  | "<="                     { MENORIGUAL }
  | "="                      { IGUAL }
  | "<>"                     { DIFERENTE }
  | "neg"                    { NEG }
  | "and"                    { E }
  | "or"                     { OU }
  | "xor"                    { XOR }
  | "not"                    { NAO }
  | "mod"                    { MOD }
  | "while"                  { WHILE }
  | "do"                     { DO }
  | "for"                    { FOR	}
  | "to"                     { TO }
  | "if"                     { IF }
  | "integer"                { TINT }
  | "real"                   { TFLOAT }
  | "bool"                   { TBOOL }
  | "char"                   { TCHAR }
  | "string"                 { TSTRING }
  | "write"                  { WRITE }
  | "readln"                 { READLN }
  | "then"                   { THEN }
  | "else"                   { ELSE }
  | "var"                    { VAR}
  | '\'' caracter '\'' as c  { CHAR (String.get c 1) }
  | '\"'(caracter+ as s)'\"' { STRING (s) }
  | booleano as bool         { BOOL (bool_of_string bool) }
  | digitofloat+ as f        { FLOAT (float_of_string f) }
  | int as num               { let numero = int_of_string num in
                               printf "INT %d\n" numero;
                               INT numero }
  | identificador as id      { ID id }
  | _ as c                   { failwith (msg_erro lexbuf c); }
  | eof                      { EOF }

and comentario n = parse
      "*/" {if n=0 then token lexbuf
              else comentario (n - 1) lexbuf }
    | "/*" { comentario (n + 1) lexbuf; }
    | _    { comentario  n lexbuf }
    | eof  {failwith "Comentários não foram fechados"}

and strings buffer = parse
    | '"'       {Buffer.contents buffer }
    | "\\t"     { Buffer.add_char buffer '\t'; strings buffer lexbuf}
    | "\\n"     { Buffer.add_char buffer '\n'; strings buffer lexbuf}
    | '\\' '"'  { Buffer.add_char buffer '"'; strings buffer lexbuf}
    | '\\' '\\' { Buffer.add_char buffer '\\'; strings buffer lexbuf}
    | _ as c    { Buffer.add_char buffer c; strings buffer lexbuf}
    | eof       { failwith "String nao foi fechada" }

 
