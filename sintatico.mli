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

val programa :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Asabs.decfn
