module S = Sintatico

open Printf;;


let print_token = let open S in
function 
    | BOOL b     -> printf "BOOL %B" b
    | INT i      -> printf "INT %d" i
    | FLOAT f    -> printf "FLOAT %f" f
    | CHAR c     -> printf "CHAR %c" c
    | STRING s   -> printf "STRING %s" s
    | ID s       -> printf "ID %s" s
    | PROGRAM    -> printf "PROGRAM"
    | BEGIN      -> printf "BEGIN"
    | END        -> printf "END"
    | FUNCAO     -> printf "FUNCAO"
    | PROCEDURE  -> printf "PROCEDURE"
    | WRITE      -> printf "WRITE"
    | READLN     -> printf "READLN"
    | VAR        -> printf "VAR"
    | TBOOL      -> printf "TBOOL"
    | TINT       -> printf "TINT"
    | TFLOAT     -> printf "TFLOAT"
    | TCHAR      -> printf "TCHAR"
    | TSTRING    -> printf "TSTRING"
    | APAR       -> printf "APAR"
    | FPAR       -> printf "FPAR"
    | ACOL       -> printf "ACOL"
    | FCOL       -> printf "FCOL"
    | PTPT       -> printf "PTPT"
    | PF         -> printf "PF"
    | VIRG       -> printf "VIRG"
    | PTVIRG     -> printf "PTVIRG"
    | OR         -> printf "OR"
    | AND        -> printf "AND"
    | NOT        -> printf "NOT"
    | TRUE       -> printf "TRUE"
    | FALSE      -> printf "FALSE"
    | MENOR      -> printf "MENOR"
    | MAIOR      -> printf "MAIOR"
    | IGUAL      -> printf "IGUAL"
    | MAIORIGUAL -> printf "MAIORIGUAL"
    | MENORIGUAL -> printf "MENORIGUAL"
    | DIFERENTE  -> printf "DIFERENTE"
    | MAIS       -> printf "MAIS"
    | MENOS      -> printf "MENOS"
    | VEZES      -> printf "VEZES"
    | DIV        -> printf "DIV"
    | MOD        -> printf "MOD"
    | ATRIB      -> printf "ATRIB"
    | IF         -> printf "IF"
    | ELSE       -> printf "ELSE"
    | THEN       -> printf "THEN"
    | DO         -> printf "DO"
    | TO         -> printf "TO"
    | WHILE      -> printf "WHILE"
    | FOR        -> printf "FOR"
    | EOF        -> printf "EOF"
