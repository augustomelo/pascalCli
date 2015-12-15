
							 
type entrada =  EntFun of (Semantico.expr_tipada Asabs.decfn)
             |  EntVar of Semantico.exprt

type t = {
  ambv : entrada Tabsimb.tabela
}

let novo_amb xs = { ambv = Tabsimb.cria xs }

let novo_escopo amb = { ambv = Tabsimb.novo_escopo amb.ambv }

let busca amb ch = Tabsimb.busca amb.ambv ch

let insere_local amb ch t =
	Tabsimb.substitui amb.ambv ch (EntVar t)

let insere_param amb ch t =
	Tabsimb.insere amb.ambv ch (EntVar t)
	
let insere_fun amb fn =
	let ef = EntFun fn
	in Tabsimb.insere amb.ambv fn.Asabs.fn_nome ef

let atualiza_var amb ch v =
	Tabsimb.atualiza amb.ambv ch (EntVar v)

