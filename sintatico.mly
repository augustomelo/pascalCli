%{
open Asabs;;
%}

%token <int> INT
%token <string> ID
%token RETURN FUNCAO
%token APAR FPAR DPTOS VIRG PTVIRG
%token PF  ATRIB
%token PROGRAM BEGIN END
%token EOF

%start programa
%type <Asabs.decfn> programa

%%
programa: funcao EOF { $1 }

funcao: PROGRAM ID PTVIRG
	BEGIN
	comandos PTVIRG
	END PF
	{
	{ fn_nome = $2;
	  (*fn_formais = $4;*)
	  fn_corpo = [$5];
	  fn_locais = []
	}
	}


comandos: RETURN ATRIB expressao { Return $3 }

expressao:  INT { Int $1 }

