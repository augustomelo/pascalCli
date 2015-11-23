type 'a programa = 'a decs
and 'a cmd = If of 'a * ('a cmd)  * (('a cmd) option)
            | While of 'a * ('a cmd)
            | For of 'a * 'a * ('a cmd)
            | Atrib of 'a * 'a
            | Bloco of ('a decs) * ('a cmds) 

and 'a cmds = 'a cmd list

and expr = ExpBool    of bool
         | ExpInt     of int
         | ExpFloat   of float
         | ExpChar    of char
         | ExpVar     of var
         | ExpString  of string
         | ExpBin     of op_binario * expr * expr
         | ExpUna     of op_unario * expr
         | ExpChamada of string * (expr list)
         | ExpTrue
		 | ExpFalse


and var = VarSimples of string 
        | VarVetor of var * expr

and op_binario =  Ou
                | E
                | Igual 
                | Maior 
                | Menor 
                | MaiorIgual 
                | MenorIgual 
                | Diferente
                | Mult 
                | Div 
                | Mais 
                | Menos
                | Mod

and op_unario = Nao
              | MenosUm

and 'a dec = DecFun of 'a decfn
           | DecVar of 'a decvar

and 'a decs = 'a dec list

and 'a decfn = { 
      fn_nome:        string;
    (*fn_formais:    (string) list;*)
      fn_locais:     'a decs;
      fn_corpo:      'a cmd;
}

and 'a decvar = {
    nome: string;
    tipo: tipo;
    valor: 'a
}

and tipo = TBool 
    | TInt 
    | TFloat 
    | TCar
    | TString
