type entrada =  EntFun of (Semantico.expr_tipada Asabs.decfn)
             |  EntVar of Semantico.exprt

type t
 
val novo_amb :  (string * entrada) list -> t
val novo_escopo : t -> t
val busca: t -> string -> entrada												 
val insere_local : t -> string -> Semantico.exprt -> unit
val insere_param : t -> string -> Semantico.exprt -> unit
val insere_fun :  t -> Semantico.expr_tipada Asabs.decfn -> unit 
val atualiza_var : t -> string -> Semantico.exprt -> unit
