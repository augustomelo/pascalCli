%{
open Asabs;;
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID 
%token TBOOL TINT TFLOAT TCHAR TSTRING 

%token MAIS MENOS VEZES DIV MOD
%token NEG MAIOR MENOR MAIORIGUAL MENORIGUAL IGUAL DIFERENTE
%token NOT AND OR XOR 

%token APAR FPAR VIRG PTVIRG PTPT
%token PF ATRIB 

%token PROGRAM BEGIN END
%token VAR
%token WRITE READLN
%token IF THEN ELSE
%token WHILE DO FOR TO
%token RETURN FUNCAO
%token EOF

%start programa
%type <Asabs.decfn> programa

%%
programa:  funcao EOF { $1 }
    
funcao :PROGRAM ID PTVIRG
        VAR declaracoes
        BEGIN
            comandos 
        END PF
        { DecFun
            { fn_nome    = $2
            (*fn_formais = $4 Parametros*)
            fn_corpo     = $5;
            (*fn_locais  = []*)
            }
        }

declaracoes: { [] }
    | declaracoes declist {$1 @ [$2]}
    ;

declist: 
    declaracaolist PTPT tipo PTVIRG {ListaDeclaracao{vars = $1, tipo = $3}}

declaracaolist: variavel VIRG declaracaolist { Dec (ExpVar $1)}
    | variavel { Dec (ExpVar $1) }
    ;

tipo: TBOOL { TBool }
    | TINT { TInt }
    | TFLOAT { TFloat }
    | TCHAR {TCar}
    | TSTRING {TString}

comandos: { [] }
    | comandos comando { $1 @ [$2]}
    ;

comando: cmd_atrib { $1 }
    | cmd_if_com_else {$1}
    | cmd_if_sem_else {$1}
    | cmd_while {$1}
    | cmd_print {$1}
    | cmd_read {$1}
    ;

cmd_atrib: expressao ATRIB expressao PTVIRG {
    CmdAtrib($1, $3)    
}

cmd_if_com_else: IF APAR expressao FPAR THEN
    BEGIN comandos END
    ELSE BEGIN comandos END PTVIRG { CmdIf($3, $7, Some($11)) }

cmd_if_sem_else: IF APAR expressao FPAR THEN
    BEGIN comandos END PTVIRG { CmdIf($3, $7, None) } 

cmd_while: WHILE APAR expressao FPAR DO
    BEGIN comandos END PTVIRG { CmdWhile($3, $7) }
 
cmd_print:
    WRITE APAR expressao FPAR  PTVIRG { CmdWrite ($3) }

cmd_read:
    READLN APAR expressao FPAR PTVIRG { CmdReadln ($3) }

expressao: expressao OR expr1 {ExpBin (Or, $1, $3) }
    | expr1 {$1};

expr1: expr1 AND expr2 {ExpBin (And, $1, $3)}
    | expr2 {$1};

expr2: expr2 XOR expr3 { ExpBin (Xor, $1, $3) }
    | expr3 {$1};

expr3: NOT expr4 { ExpUna (Not, $2) }
    | expr4 {$1};

expr4: expr4 IGUAL expr5 { ExpBin (Igual, $1, $3) }
    | expr5 {$1};

expr5: expr5 MAIOR expr6 { ExpBin (Maior, $1, $3) }
    | expr6 { $1 };

expr6: expr6 MENOR expr7 { ExpBin (Menor, $1, $3) }
    | expr7 { $1 };
/
expr7: expr7 MAIORIGUAL expr8 { ExpBin(MaiorIgual, $1, $3) }
    | expr8 { $1 };

expr8: expr8 MENORIGUAL expr9 { ExpBin (MenorIgual, $1, $3) }
    | expr9 { $1 };

expr9: expr9 VEZES expr10 { ExpBin (Mult, $1, $3) }
    | expr10 { $1 };

expr10: expr10 DIV expr11 { ExpBin (Div, $1, $3) }
    | expr11 { $1 };

expr11: expr11 MAIS expr12 { ExpBin (Mais, $1, $3) }
    | expr12 { $1 };

expr12: expr12 MENOS expr13 { ExpBin (Menos, $1, $3) }
    | expr13 { $1 };

expr13: expr13 MOD expr14 { ExpBin (Mod, $1, $3) }
    | expr14 { $1 };

expr14: NEG expr15 { ExpUna (Neg, $2) }
    | expr15 { $1 };

expr15: expr15 DIFERENTE expr16 { ExpBin (Diferente, $1, $3) }
    | expr16 {$1};

expr16: operando { $1 }
    | APAR expressao FPAR { $2 }
    | variavel { (ExpVar $1) };


operando: BOOL { ExpBool $1 }
    | INT { ExpInt $1 }
    | FLOAT { ExpFloat $1 }
    | CHAR { ExpChar $1 }
    | STRING { ExpString $1 }
    ;

variavel: ID { VarSimples $1}
    ;



