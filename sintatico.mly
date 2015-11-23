%{
open Asabs;;
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID 
%token TBOOL TINT TFLOAT TCHAR TSTRING TRUE FALSE

%token MAIS MENOS VEZES DIV MOD
%token MAISATRIB INCR
%token NEG MAIOR MENOR MAIORIGUAL MENORIGUAL IGUAL DIFERENTE
%token NAO E OU XOR 

%token APAR FPAR VIRG PTVIRG PTPT ACOL FCOL
%token PF ATRIB  ACOL FCOL

%token PROGRAM BEGIN END
%token VAR
%token WRITE READLN
%token IF THEN ELSE
%token WHILE DO FOR TO
%token RETURN FUNCAO
%token EOF

%start programa
%type <Asabs.expr Asabs.programa> programa

%%
programa: funcao EOF { [$1] }
    
funcao: PROGRAM ID PTVIRG
        declara_variaveis
        BEGIN
            comandos
        END PF
        { 
            DecFun { fn_nome    = $2;
                   (*fn_formais = $4 Parametros*)
                     fn_locais  = $4;
                     fn_corpo   = $6;
            }
        }

declara_variaveis: { [] }
    | VAR declaracoes { $2 }

declaracoes: { [] }
    | declaracoes declist {$1 @ $2}
    ;

declist: 
    declaracaolist PTPT tipo PTVIRG {
        List.map (fun n -> DecVar {nome = n; tipo = $3; valor = ExpInt 0}) $1
    }

declaracaolist: variavel VIRG declaracaolist { $1 :: $3 } 
    | variavel { [$1] }
    ;

tipo: TBOOL   { TBool }
    | TINT    { TInt }
    | TFLOAT  { TFloat }
    | TCHAR   { TCar}
    | TSTRING { TString}


variavel: ID { $1 }
    ;


comandos:  inicia_variaveis lista_de_comandos { Bloco ($1, $2) }

inicia_variaveis: { [] }
    | inicia_variaveis atribui_variaveis { $1 @ [$2] }

atribui_variaveis: ID ATRIB expressao PTVIRG { 
   DecVar { nome = $1; tipo = TInt; valor = $3} 
}


lista_de_comandos: { [] }
    | lista_de_comandos comando { $1 @ [$2] }

comando: cmd_selecao    { $1 }
       | cmd_atribuicao { $1 }
       | comandos       { $1 }
       | cmd_iteracao   { $1 }

cmd_selecao: IF APAR expressao FPAR comando { If ($3, $5, None)  }
       | IF APAR expressao FPAR comando ELSE comando  { If ($3, $5, Some $7)  }
       /* acrescentar switch case */

cmd_atribuicao: cmd_atrib PTVIRG { $1 }

cmd_atrib: mutavel ATRIB expressao    { Atrib ($1, $3) }
       | mutavel MAISATRIB expressao  { Atrib ($1, ExpBin (Mais, $1, $3))  }
       | mutavel INCR  { Atrib ($1, ExpBin (Mais, $1, ExpInt 1)) }
       /* Completar com outros operadores de atribuição */

lista_de_comandos:  { [] }
       | lista_de_comandos comando { $1 @ [$2] }

cmd_iteracao: WHILE APAR expressao FPAR comando { While ($3, $5)  }
       | FOR mutavel TO expressao comando { For ($2, $4, $5) } 
       /* Acrescentar do-while */

expressao: exp_ou  { $1 }

exp_ou: exp_ou OU exp_e { ExpBin(Ou, $1, $3) }
			| exp_e           { $1 }

exp_e: exp_e E exp_not { ExpBin(E, $1, $3) }
     | exp_not         { $1 }

exp_not: NAO exp_not    { ExpUna(Nao, $2) }
  		 | exp_relacional { $1 }

exp_relacional: exp_soma op_rel exp_soma { ExpBin($2, $1, $3) }
			 | exp_soma { $1 }

op_rel: MENOR      { Menor }
	 | MAIOR       { Maior }
     /* acrescentar outros operadores relacionais */

exp_soma: exp_soma op_soma parcela { ExpBin($2, $1, $3) }
			  | parcela  { $1 }

op_soma: MAIS  { Mais  }
	   | MENOS { Menos }

parcela: parcela op_mult exp_unaria { ExpBin($2, $1, $3) }
	   | exp_unaria   { $1 }

op_mult: VEZES { Mult }
	   | DIV   { Div  }
	   | MOD   { Mod  }

exp_unaria: op_unario exp_unaria { ExpUna($1, $2) }
		 | fator  { $1 }

op_unario: MENOS { MenosUm }

fator: imutavel { $1 }
	 | mutavel  { $1 }

mutavel: ID   {  ExpVar (VarSimples $1) }
	   | ID ACOL expressao FCOL { ExpVar (VarVetor (VarSimples $1, $3)) }

imutavel: APAR expressao FPAR { $2 }
		 | chamada   { $1 }
		 | constante { $1 }

chamada: ID APAR args FPAR { ExpChamada ($1, $3) }

args: /* vazio */     { [] }
		 | lista_de_args  { $1 }

lista_de_args: lista_de_args VIRG expressao { $1 @ [$3] }
		 | expressao { [ $1 ] }

constante: INT { ExpInt $1    }
    | FLOAT    { ExpFloat $1 }
    | CHAR     { ExpChar $1 }
    | STRING   { ExpString $1 }
    | TRUE     { ExpTrue      } /* a sua linguagem podem não ter valores booleanos */
    | FALSE    { ExpFalse     }


operando: BOOL { ExpBool $1 }
    | INT      { ExpInt $1 }
    | FLOAT    { ExpFloat $1 }
    | CHAR     { ExpChar $1 }
    | STRING   { ExpString $1 }
    ;







