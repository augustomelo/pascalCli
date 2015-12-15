type 'a programa = 'a decs

and 'a cmd = CmdReturn  of 'a option
    |        CmdIf      of 'a * ('a cmd)  * (('a cmd) option)
    |        CmdWhile   of 'a * ('a cmd)
    |        CmdAtrib   of 'a * 'a
    |        CmdChamada of 'a
    |        Bloco      of ('a decs) * ('a cmds)


and 'a cmds = 'a cmd list

and expr = ExpInt     of int
    |      ExpFloat   of float
    |      ExpChar    of char
    |      ExpString  of string
    |      ExpTrue
    |      ExpFalse
    |      ExpVar     of var
    |      ExpBin     of op_binario * expr * expr
    |      ExpUna     of op_unario * expr
    |      ExpChamada of string * (expr list)

and var = VarSimples of string 
        | VarVetor   of  var * expr

and op_binario =  Or
    |             And
    |             Igual 
    |             Maior 
    |             Menor 
    |             MaiorIgual 
    |             MenorIgual 
    |             Diferente
    |             Mult 
    |             Div 
    |             Mais 
    |             Menos
    |             Mod

and op_unario = Not
    |           MenosUm

and 'a dec = DecFun of 'a decfn
    |        DecVar of 'a decvar

and 'a decs = 'a dec list

and 'a decfn = { 
      fn_nome:     string;
      fn_formais: (string * tipo) list;
      fn_tiporet:  tipo option;
      fn_corpo:   'a cmd
}

and 'a decvar = {
    nome:  string;
    tipo:  tipo;
    valor: 'a
}

and tipo = TBool 
    |      TInt  
    |      TFloat 
    |      TChar
    |      TString
    |      TVoid
