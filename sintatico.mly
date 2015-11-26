%{
open Asabs;;
%}

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <char>   CHAR
%token <string> STRING
%token <string> ID 
%token TBOOL TINT TFLOAT TCHAR TSTRING 

%token MAIS MENOS VEZES DIV MOD
%token NEG MAIOR MENOR MAIORIGUAL MENORIGUAL IGUAL DIFERENTE
%token NOT AND OR  
%token TRUE FALSE

%token APAR FPAR ACOL FCOL VIRG PTVIRG PTPT
%token PF ATRIB 

%token PROGRAM BEGIN END
%token VAR
%token WRITE READLN
%token IF THEN ELSE
%token WHILE DO FOR TO
%token RETURN FUNCAO PROCEDURE
%token EOF

%start programa
%type <Asabs.expr Asabs.programa> programa

%%
programa: main EOF { $1 }
    
main: PROGRAM ID PTVIRG
        declaracoesvars
        declarafuncoes
      BEGIN
        lista_de_comandos
      END PF
      { 
          Programa ($2, $4, $5, $7) 
      }

declaracoesvars: { [] }
    |            VAR declaracoes { $2 } 
    ;

declaracoes: { [] }
    |        declaracoes declist {$1 @ $2}
    ;

declist: declaracaolist PTPT tipo PTVIRG {
            List.map (fun n -> DecVar {nome = n; tipo = Some $3; valor = ExpInt 0}) $1 
         }

declaracaolist: variavel VIRG declaracaolist { $1 :: $3 } 
    |           variavel { [$1] }
    ;

variavel: ID { $1 }
    ;

declarafuncoes: { [] }
    |           declarafuncoes declarafuncao { $1 @ [$2] }

declarafuncao: FUNCAO ID  APAR argumentos FPAR tipo_retorno_opc PTVIRG
               escopo_func
               { 
                   DecFun { 
                           fn_nome    = $2;
                           fn_formais = $4;
                           fn_tiporet = $6;
                           fn_corpo   = $8;
                   }
               }
    |          PROCEDURE ID  APAR argumentos FPAR PTVIRG
               escopo_func
               { 
                   DecFun { 
                           fn_nome    = $2;
                           fn_formais = $4;
                           fn_tiporet = None;
                           fn_corpo   = $7;
                   }
               }


argumentos: { [] }
    |       argumento resto_argumentos { $1 :: $2 }

resto_argumentos: { [] }
    |             PTVIRG argumentos  { $2 }

argumento: ID PTPT tipo { ($1, $3) }


tipo_retorno_opc: PTPT tipo { Some $2 }

tipo: TBOOL   { TBool   }
    | TINT    { TInt    }
    | TFLOAT  { TFloat  }
    | TCHAR   { TChar   }
    | TSTRING { TString }

escopo_func: declaracoesvars
              BEGIN
                lista_de_comandos
              END PTVIRG { Bloco ($1, $3) }

lista_de_comandos: { [] }
    | lista_de_comandos comandos { $1 @ [$2] }

comandos: cmd_atribuicao { $1 }
    |     cmd_selecao    { $1 }
    |     cmd_iteracao   { $1 }
    ;

cmd_atribuicao: cmd_atrib PTVIRG { $1 }

cmd_atrib: mutavel ATRIB expressao { CmdAtrib($1, $3) }

cmd_selecao: IF APAR expressao FPAR THEN 
             BEGIN 
                comandos
             END PTVIRG { CmdIf ($3, $7, None)  }
    |        IF APAR expressao FPAR THEN 
             BEGIN 
                comandos
             END 
             ELSE 
             BEGIN 
                comandos  
             END PTVIRG { CmdIf ($3, $7, Some $11)  }


cmd_iteracao: WHILE APAR expressao FPAR DO
              BEGIN
                comandos
              END PTVIRG { CmdWhile($3, $7) }

expressao: exp_or { $1 }

exp_or: exp_or OR exp_and { ExpBin(Or, $1, $3) }
    |   exp_and           { $1 }

exp_and: exp_and AND exp_not { ExpBin(And, $1, $3) }
    |    exp_not             { $1 }

exp_not: NOT exp_not   {ExpUna(Not, $2)}
    | exp_relacional { $1 }

exp_relacional: exp_soma op_rel exp_soma { ExpBin($2, $1, $3) }
    |           exp_soma                 { $1 }

exp_soma: exp_soma op_soma parcela { ExpBin($2, $1, $3) }
    |     parcela                  { $1 }

op_soma: MAIS  { Mais  }
    |    MENOS { Menos }

op_rel: IGUAL      { Igual      }
    |   MAIOR      { Maior      }
    |   MENOR      { Menor      }
    |   MAIORIGUAL { MaiorIgual }
    |   MENORIGUAL { MenorIgual }
    |   DIFERENTE  { Diferente  }

parcela: parcela op_mult exp_unaria { ExpBin ($2, $1, $3) }
    |    exp_unaria                 { $1 }

op_mult: VEZES { Mult }
    |    DIV   { Div }
    |    MOD   { Mod }

exp_unaria: op_unario exp_unaria { ExpUna($1, $2) }
    |       fator                { $1 }

op_unario: MENOS { MenosUm }

fator:  imutavel { $1 }
    |   mutavel  { $1 }

mutavel: ID                     { ExpVar (VarSimples $1) }
	|    ID ACOL expressao FCOL { ExpVar (VarVetor (VarSimples $1, $3)) }

imutavel: APAR expressao FPAR { $2 }
	|     chamada   { $1 }
    |     constante { $1 }

chamada: ID APAR args FPAR PTVIRG { ExpChamada ($1, $3) }

args:               { [] }
    | lista_de_args { $1 }

lista_de_args: lista_de_args VIRG expressao { $1 @ [$3] }
    |          expressao                    { [ $1 ] }


constante: INT    { ExpInt    $1 }
    |      FLOAT  { ExpFloat  $1 }
    |      CHAR   { ExpChar   $1 }
    |      STRING { ExpString $1 }
    |      TRUE   { ExpTrue      }
    |      FALSE  { ExpFalse     }









