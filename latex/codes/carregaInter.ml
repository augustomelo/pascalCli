#load "asabs.cmo";;
#load "sintatico.cmo";;  
#load "imprime.cmo"
#load "lexico.cmo";;
#load "tabsimb.cmo";;
#load "ambiente.cmo";;
#load "semantico.cmo";;
#load "ambInterpretador.cmo";;

#load "interprete.cmo";;

open Printf;;

  
open Asabs;;
open AmbInterpretador;;

let sintatico lexbuf =
  try
    Sintatico.programa Lexico.token lexbuf
  with exn ->
    begin
      let tok  = Lexing.lexeme lexbuf in
      let pos  = lexbuf.Lexing.lex_curr_p in
      let nlin = pos.Lexing.pos_lnum in
      let ncol = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length tok in
      let msg1 = sprintf "Erro na linha %d, coluna %d" nlin ncol in
      let msg2 = sprintf "\tA palavra \"%s\" nao era esperada aqui." tok in
        print_endline msg1;
        print_endline msg2;
        flush stdout;
        raise exn
    end

let anasint_da_string str =
  let lexbuf = Lexing.from_string str in
    sintatico lexbuf

let anasint arq =
  let ic = open_in arq in
  let lexbuf = Lexing.from_channel ic in
  let asa = sintatico lexbuf in
  let _ = close_in ic in
  asa

let anasem_da_string str =
	let asa = anasint_da_string str in
  let (tab, at) = Semantico.semantico asa in
   (tab, at)

let anasem arq =
	let asa = anasint arq in
  let (tab, at) = Semantico.semantico asa in
   (tab, at)

let interpreta arq =
  let (tab, at) = anasem arq in
  Interprete.interprete at
