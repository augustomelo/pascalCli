module Amb = Ambiente
module A = Asabs


type expr_tipada = {
   exp : exprt;
   tinf: A.tipo
}

and exprt = ExpInt     of int
    |       ExpFloat   of float
    |       ExpChar    of char
    |       ExpString  of string
    |       ExpTrue
    |       ExpFalse
    |       ExpVar     of A.var
    |       ExpBin     of A.op_binario * expr_tipada * expr_tipada
    |       ExpUna     of A.op_unario * expr_tipada
    |       ExpChamada of string * (expr_tipada list)
    |       ExpVoid

let exp2str e =
  match e with ExpInt n -> string_of_int n
    |          ExpFloat n -> string_of_float n
	|          ExpString s -> s
	|          ExpTrue  -> "true"
	|          ExpFalse -> "false"
	|          ExpVoid  -> "()"
    |          _ -> "outras expressoes"

let nome_tipo t =
  let open A in
	match t with
        TBool   -> "TBool"
      | TInt    -> "TInt"
      | TFloat  -> "TFloat"
      | TChar   -> "TChar"
      | TString -> "TString"
      | TVoid   -> "TVoid"

type classe_op = Aritmetico | Relacional | Logico

let classifica op =
  let open A in
  match op with
      Or 
	| And  -> Logico
	| Menor  
	| Maior
	| Igual
	| MaiorIgual
	| MenorIgual
	| Diferente -> Relacional
	| Mais
	| Menos
	| Mult
	| Div
	| Mod -> Aritmetico

							 
let confere_tipo msg tinf tdec =
 	if tinf <> tdec
	then failwith (msg ^ " deve ser do tipo " ^ tinf)

let tipos_iguais msg tinf tdec =
	if tinf <> tdec
	then
		failwith (Printf.sprintf msg (nome_tipo tinf) (nome_tipo tdec))

let rec verifica_dup xs =
  match xs with
    [] -> false
  | (nome,_)::xs -> 
    if (List.for_all (fun (n,t) -> n <> nome) xs)
    then verifica_dup xs
    else failwith ("Parâmetro duplicado " ^ nome)

let analisa_dec amb dec =
  let open A in
	match dec with
      DecVar {nome; tipo} ->  Amb.insere_local amb nome tipo
	| DecFun {fn_nome; fn_tiporet; fn_formais; fn_corpo} -> 
        (* Verifica se não há parâmetros duplicados *)
        let _ = verifica_dup fn_formais in
        Amb.insere_fun amb fn_nome fn_formais fn_tiporet


let rec verifica_cmd amb tiporet cmd =
    let open A in
    match cmd with
         CmdReturn exp -> 
            (match exp with
                None -> let _ = tipos_iguais "O tipo retornado é %s mas foi declarado como %s" 
                            TVoid tiporet in CmdReturn None
                | Some e -> 
                    let exp_tip = verifica_exp amb e in
                    let _ = tipos_iguais "O tipo retornado é %s mas foi declarado como %s" 
                            exp_tip.tinf tiporet 
                    in CmdReturn (Some exp_tip)
                )
        | CmdIf (teste, entao, senao) ->
            let teste_tip = verifica_exp amb teste in
            let _ = tipos_iguais "O teste do if deveria ser do tipo %s e não %s" 
                    TBool teste_tip.tinf in
            let entao_tip = verifica_cmd amb tiporet entao in
            let senao_tip =       
              match senao with
                None -> None
              | Some bloco -> Some (verifica_cmd amb tiporet bloco) in
                CmdIf (teste_tip, entao_tip, senao_tip)
        | CmdWhile (teste, corpo) ->
            let teste_tip = verifica_exp amb teste in
            let _ = tipos_iguais "O teste do if deveria ser do tipo %s e não %s" 
                        TBool teste_tip.tinf in
            let corpo_tip = verifica_cmd amb tiporet corpo in
            CmdWhile (teste_tip, corpo_tip)
        | CmdAtrib (elem, exp) ->
           (*let elem_tip = verifica_exp amb elem
           and exp_tip = verifica_exp amb exp in
           let _ = tipos_iguais "Atribuicao com tipos diferentes: %s = %s" 
                      elem_tip.tinf exp_tip.tinf 
           in CmdAtrib (elem_tip, exp_tip) 
            *)
            let  exp2 = verifica_exp amb exp in
            (match elem with
                A.ExpVar v ->  
                ( match v with 
                  A.VarSimples nome -> 
                  ( try (match (Amb.busca amb nome) with
                            Amb.EntVar tipo -> let elem_tip = verifica_exp amb elem
                                and exp_tip = verifica_exp amb exp in
                                let _ = tipos_iguais "Atribuicao com tipos diferentes: %s = %s" 
                                    elem_tip.tinf exp_tip.tinf 
                                    in CmdAtrib (elem_tip, exp_tip)

                          | Amb.EntFun { tipo_fn; _ } -> ( match tipo_fn with
                                        None ->  let _ = tipos_iguais "Funcao do tipo %s nao pode receber o valor do tipo %s"
                                            TVoid exp2.tinf in
                                            CmdReturn (Some {exp = ExpVoid; tinf = A.TVoid})
                                      | Some tipo ->  let _ = tipos_iguais "Funcao do tipo %s nao pode receber o valor do tipo %s" 
                                            tipo exp2.tinf in 
                                            CmdReturn (Some exp2)
                                              
                                    )
                        ) 
                    with Not_found -> failwith ("A variável " ^ nome ^ " não foi declarada")
                  )
                | A.VarVetor _ -> failwith "verifica_exp: vetor não implementado"
                )
              | _ ->  let elem_tip = verifica_exp amb elem
                     and exp_tip = verifica_exp amb exp in
                     let _ = tipos_iguais "Atribuicao com tipos diferentes: %s = %s" 
                             elem_tip.tinf exp_tip.tinf 
                             in CmdAtrib (elem_tip, exp_tip)

            )

        | CmdChamada exp -> 
            let exp_tip = verifica_exp amb exp in
            CmdChamada exp_tip
        | Bloco (decs, cmds) ->
           let amb_bloco = Amb.novo_escopo amb in
           let decs_tip = List.map (verifica_dec amb_bloco) decs in
           let _ = List.iter (analisa_dec amb_bloco) decs_tip in
           let cmds_tip = List.map (verifica_cmd amb_bloco tiporet) cmds in
           Bloco (decs_tip, cmds_tip)


and verifica_exp amb exp =
    match exp with
	      A.ExpInt n    -> { exp = ExpInt n;    tinf = A.TInt }
        | A.ExpFloat n  -> { exp = ExpFloat n;  tinf = A.TFloat }
        | A.ExpString s -> { exp = ExpString s; tinf = A.TString }
        | A.ExpChar c   -> { exp = ExpChar c;   tinf = A.TChar }
        | A.ExpTrue     -> { exp = ExpTrue;     tinf = A.TBool }
        | A.ExpFalse    -> { exp = ExpFalse;    tinf = A.TBool }
        | A.ExpVar v ->
            (match v with
               A.VarSimples nome -> 
                 (try (match (Amb.busca amb nome) with 
                        Amb.EntVar tipo -> { exp = ExpVar v; tinf = tipo}
                      | Amb.EntFun _ -> 
                            failwith ("Nao era para acontecer")
                          (*failwith ("nome de função usado como nome de variável: " ^ nome)*)
                     )
                 with Not_found -> 
                         failwith ("A variável " ^ nome ^ " não foi declarada")
                 )
             | A.VarVetor _ -> failwith "verifica_exp: vetor não implementado"
            )
        | A.ExpBin (op, esq, dir) ->
          let esq_tip = verifica_exp amb esq
          and dir_tip = verifica_exp amb dir in
          let verifica_aritmetico () =
            (match esq_tip.tinf with
               A.TInt 
             | A.TFloat -> tipos_iguais 
                         "O operando esquerdo é do tipo %s mas o direito é do tipo %s" 
                         esq_tip.tinf dir_tip.tinf 
             | t -> failwith ("um operador aritmético não pode ser usado com o tipo " ^
                              (nome_tipo t))
            )
          and verifica_relacional () =
            (match esq_tip.tinf with
               A.TInt 
             | A.TFloat 
             | A.TString -> tipos_iguais 
                         "O operando esquerdo é do tipo %s mas o direito é do tipo %s" 
                         esq_tip.tinf dir_tip.tinf 
             | t -> failwith ("um operador relacional não pode ser usado com o tipo " ^
                              (nome_tipo t))
            ) 
          and verifica_logico () =
            (match esq_tip.tinf with
               A.TBool -> tipos_iguais 
                         "O operando esquerdo é do tipo %s mas o direito é do tipo %s" 
                         esq_tip.tinf dir_tip.tinf 
             | t -> failwith ("um operador lógico não pode ser usado com o tipo " ^
                              (nome_tipo t))
            )   
          in
          let tipo_exp = (match (classifica op) with
                     Aritmetico -> let _ = verifica_aritmetico () in esq_tip.tinf
                   | Relacional -> let _ = verifica_relacional () in A.TBool
                   | Logico     -> let _ = verifica_logico     () in A.TBool
                  ) 
          in
            { exp = ExpBin (op, esq_tip, dir_tip ); tinf = tipo_exp }
        | A.ExpUna (op, exp) ->
           let exp_tip = verifica_exp amb exp in
           let verifica_aritmetico () =
            (match exp_tip.tinf with
               A.TInt 
             | A.TFloat -> ()
             | t -> failwith ("um operador aritmético não pode ser usado com o tipo " ^
                              (nome_tipo t))
            )
           in
           let _ = (match op with
                      A.Not -> tipos_iguais 
                         "O operando é do tipo %s mas deveria ser do tipo %s" 
                         exp_tip.tinf A.TBool 
                    | A.MenosUm -> verifica_aritmetico ()
                   ) 
           in {exp = ExpUna (op, exp_tip); tinf = exp_tip.tinf}
        | A.ExpChamada (nome, args) ->
           let rec verifica_tipos ps fs =
              match (ps, fs) with
               (p::ps), (f::fs) -> 
                  let _ = tipos_iguais 
                           "Parâmetro do tipo %s mas deveria ser do tipo %s" p f in
                  verifica_tipos ps fs
             | [], [] -> ()
             | _ -> failwith "Número incorreto de parâmetros"
           in
           try 
             begin
               let open Amb in
               match (Amb.busca amb nome) with
                 Amb.EntFun {tipo_fn; formais} -> 
                  let tipo_func = match tipo_fn with
                    | None -> A.TVoid
                    | Some tipo -> tipo in
                  let args_tip = List.map (verifica_exp amb) args 
                  and tipos_formais = List.map snd formais in
                    let _ = verifica_tipos (List.map (fun {tinf} -> tinf) args_tip) 
                                tipos_formais 
                    in { exp = ExpChamada (nome, args_tip); tinf = tipo_func }
               | Amb.EntVar _ -> failwith (nome ^ " é uma variável e não uma função")       
             end
           with Not_found -> failwith ("Não existe a função " ^ nome)

and verifica_dec amb asa =
    let open A in
    match asa with
    	  A.DecVar {nome; tipo; valor} ->
      	    let exp_tipada = verifica_exp amb valor in
                let _ = tipos_iguais "Variável do tipo %s não pode receber valor do tipo %s"
      			    exp_tipada.tinf tipo in
                        A.DecVar {nome; tipo; valor = exp_tipada} 
        | A.DecFun {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
            let tipo_func = match fn_tiporet with
                | None -> A.TVoid
                | Some tipo -> tipo in 
            let ambfn = Amb.novo_escopo amb in
                let _ = List.iter (fun (v,t) -> Amb.insere_param ambfn v t) fn_formais in
                    let corpo_tipado = verifica_cmd ambfn tipo_func fn_corpo in
                        A.DecFun {fn_nome; fn_tiporet; fn_formais; fn_corpo = corpo_tipado}

let fn_predefinidas = let open A in [
   ("printInt",    [("x", TInt)],      Some TVoid);
   ("printString", [("x", TString)],   Some TVoid);
   ("printFloat",  [("x", TFloat)],    Some TVoid);
   ("sqrt",        [("x", TFloat)],    Some TFloat)
]


let dec_fn_predefinidas amb =   
  List.iter (fun (n,ps,tr) -> Amb.insere_fun amb n ps tr) fn_predefinidas

			
let semantico asa =
	let amb = Amb.novo_amb [] in
        let _ = dec_fn_predefinidas amb in
            let _ = List.iter (analisa_dec amb) asa in
                let arv = List.map (verifica_dec amb) asa in
                    (amb, arv)

(* 

		  let exp_tipada = tipifica_expr amb valor in
      let _ = (tipos_iguais 
          			 "Variável do tipo %s não pode receber tipo %s"
							   exp_tipada.tinf tipo; 


*)
