type expr_tipada = {
   exp : exprt;
   tinf: Asabs.tipo
}

and exprt = ExpInt     of int
    |       ExpFloat   of float
    |       ExpChar    of char
    |       ExpString  of string
    |       ExpTrue
    |       ExpFalse
    |       ExpVar     of Asabs.var
    |       ExpBin     of Asabs.op_binario * expr_tipada * expr_tipada
    |       ExpUna     of Asabs.op_unario * expr_tipada
    |       ExpChamada of string * (expr_tipada list)
    |       ExpVoid

val exp2str : exprt -> string
val nome_tipo : Asabs.tipo -> string 

val fn_predefinidas : (string * (string * Asabs.tipo) list *  Asabs.tipo option) list
val semantico : Asabs.expr Asabs.dec list -> Ambiente.t * expr_tipada Asabs.dec list



