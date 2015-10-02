open Asabs;;
open Asadec;;
open Tabsimb;;
  
let preenche_tab_simbolos amb asa = ()

let verifica_exp amb exp =
  match exp with
    Int e -> ConstInt e

let verifica_cmd amb asa =
  match asa with
    Return e -> Ret (verifica_exp amb e)

let verifica_fn amb {fn_corpo} =
	List.map (verifica_cmd amb) fn_corpo

let verifica_tipo amb asa =
	verifica_fn amb asa

let semantico asa =
  let (ambiente:tabSimbolos) = Hashtbl.create 23 in
  let _ = preenche_tab_simbolos ambiente asa in
  let asadec = verifica_tipo ambiente asa in
  (asadec, ambiente)
