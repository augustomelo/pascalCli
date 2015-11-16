type 'a programa = 'a decs
and 'a cmd = CmdIf of 'a * ('a cmd)  * (('a cmd) option)
           | CmdWhile of 'a * ('a cmd)
           | CmdAtrib of 'a * ('a cmd)

and 'a cmds = 'a cmd list

and expr = ExpBool of bool
         | ExpInt of int
         | ExpFloat of float
         | ExpChar of char
         | ExpVar of var
         | ExpString of string
         | ExpBin of op_binario * expr * expr
         | ExpUna of op_unario * expr

and var = Dec of string

and op_binario =  Or
                | And
                | Xor 
                | Igual 
                | Maior 
                | Menor 
                | MaiorIgual 
                | MenorIgual 
                | Mult 
                | Div 
                | Mais 
                | Menos
                | Mod

and op_unario = Not
              | Neg

and 'a dec = DecFun of 'a decfn
           | ListaDeclaracao of 'a decvar

and 'a decs = 'a dec list

and 'a decfn = { 
    fn_nome: string;
    (*fn_formais: (string) list;*)
    fn_corpo: 'a cmd;
    (*fn_locais: (string) list;*)
}

and 'a decvar = {
    vars: (string) list;
    tipo: string
}
