type entrada_fn = { tipo_fn:  Asabs.tipo option;
                    formais: (string * Asabs.tipo) list;
					(*      locais: (string * Asabs.tipo) list *)
                  }

type entrada =  EntFun of entrada_fn
    |           EntVar of Asabs.tipo

type t
 
val novo_amb :  (string * entrada) list -> t
val novo_escopo : t -> t
val busca: t -> string -> entrada												 
val insere_local : t -> string -> Asabs.tipo -> unit
val insere_param : t -> string -> Asabs.tipo -> unit
val insere_fun : t -> string -> (string * Asabs.tipo) list -> Asabs.tipo option -> unit
