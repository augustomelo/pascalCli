module Amb = AmbInterpretador
module A = Asabs
module S = Semantico


exception Valor_retornado of S.exprt

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

let obtem_i32 e = (match e with S.ExpInt v -> v 
                              | _ -> failwith "obtem_i32: falha")

let obtem_f32 e = (match e with S.ExpFloat v -> v 
                              | _ -> failwith "obtem_f32: falha")

let obtem_bool e = (match e with S.ExpTrue -> true | S.ExpFalse -> false 
                                 | _ -> failwith "obtem_bool: falha")

let obtem_string e = (match e with S.ExpString s -> s
                                 | _ -> failwith "obtem_string: falha")    

let funcoes_especiais =  ref [ "printInt"; "printString"; "printFloat"; "sqrt" ]

  
let rec insere_dec amb dec =
  let open A in
	match dec with
		DecVar {nome; valor} -> 
       Amb.insere_local amb nome (avalia_exp amb valor)
	| DecFun fn ->  Amb.insere_fun amb fn

and avalia_cmd amb cmd =
  let open A in
  match cmd with
    CmdReturn exp -> 
      (match exp with
         None -> S.ExpVoid
       | Some e -> let resultado = avalia_exp amb e in
                   raise (Valor_retornado resultado)
      )
  | CmdIf (teste, entao, senao) ->
      let resultado_teste = avalia_exp amb teste in
      (match resultado_teste with
        S.ExpTrue -> avalia_cmd amb entao
      | S.ExpFalse -> 
        (match senao with
          None -> S.ExpVoid
        | Some bloco -> avalia_cmd amb bloco
        )
      | _ -> failwith "avalia_cmd if: valor nao booleano"
      )

  | CmdWhile (teste, corpo) -> 
      let resultado_teste = avalia_exp amb teste in
      (match resultado_teste with
        S.ExpTrue -> let _ = avalia_cmd amb corpo in avalia_cmd amb cmd
      | S.ExpFalse -> S.ExpVoid
      | _ -> failwith "avalia_cmd while: valor nao booleano"
      )

  (*| For _ -> failwith "avalia_cmd for: comando nao foi implementado"*)
  | CmdAtrib (elem, expr) ->  
     let obtem_nome () = 
       (match elem.S.exp with 
          S.ExpVar v -> (match v with
                         A.VarSimples nome -> nome
                       | A.VarVetor _ -> 
                           failwith "avalia_cmd: vetor nao implementado"
                      )
        | _ -> failwith "avalia_cmd atrib: não é variavel"
       )
     in
     let nome_var = obtem_nome()
     and valor_exp = avalia_exp amb expr in
     let _ = Amb.atualiza_var amb nome_var valor_exp in
     valor_exp

  | Bloco (decs, cmds) -> 
     let amb_bloco =  if (List.length decs == 0) then amb else Amb.novo_escopo amb in
     let _ = List.iter (insere_dec amb_bloco) decs in
     let resultado = avalia_cmds amb_bloco cmds in
     resultado

  | CmdChamada exp -> avalia_exp amb exp

and avalia_cmds amb cmds =
  match cmds with
    (*((A.Return _) as cmd) :: _ -> let resultado = avalia_cmd amb cmd in
                                  raise (Valor_retornado resultado)*)
  | cmd::cmds ->  let _ = avalia_cmd amb cmd in avalia_cmds amb cmds
  | [] -> S.ExpVoid


and avalia_exp amb {S.exp} =
	match exp with
	S.ExpInt _    
  | S.ExpFloat _  
  | S.ExpChar _
  | S.ExpString _ 
  | S.ExpTrue     
  | S.ExpFalse    
  | S.ExpVoid     -> exp
  | S.ExpVar v ->
    (match v with
       A.VarSimples nome -> 
         (match Amb.busca amb nome with 
                Amb.EntVar valor -> valor
              |  _ -> failwith ("avalia_exp: nunca acontece")
          )
     | A.VarVetor _ -> failwith "avalia_exp: vetor nao implementado"
    )
  | S.ExpBin (op, esq, dir) ->
    let res_esq = avalia_exp amb esq
    and res_dir = avalia_exp amb dir in
    let avalia_aritmetico () =      
      (match esq.S.tinf with
         A.TInt -> let arg1 = obtem_i32 res_esq 
                   and arg2 = obtem_i32 res_dir in
                   S.ExpInt (match op with
	                    A.Mais  -> arg1 + arg2
	                  | A.Menos -> arg1 - arg2
	                  | A.Mult  -> arg1 * arg2
	                  | A.Div   -> arg1 / arg2
	                  | A.Mod   -> arg1 mod arg2
                    | _ -> failwith "avalia_aritmetico: operacao invalida"
                   ) 
       | A.TFloat -> let arg1 = obtem_f32 res_esq 
                   and arg2 = obtem_f32 res_dir in
                   S.ExpFloat (match op with
	                    A.Mais  -> arg1 +. arg2
	                  | A.Menos -> arg1 -. arg2
	                  | A.Mult  -> arg1 *. arg2
	                  | A.Div   -> arg1 /. arg2
                      | _ -> failwith "avalia_aritmetico: operacao invalida"
                   ) 
       | _ -> failwith "avalia_aritmetico: operacao invalida"
      )
    and avalia_relacional () =
      (match esq.S.tinf with
         A.TInt -> let arg1 = obtem_i32 res_esq 
                   and arg2 = obtem_i32 res_dir in
                   if (match op with
	                          | A.Menor -> arg1 < arg2
	                          | A.Maior -> arg1 > arg2
	                          | A.Igual -> arg1 == arg2
	                          | A.MaiorIgual -> arg1 >= arg2
	                          | A.MenorIgual -> arg1 <= arg2
	                          | A.Diferente  -> arg1 != arg2
                              | _ -> 
                              failwith "avalia_relacional: operacao int invalida"
                           )
                   then S.ExpTrue
                   else S.ExpFalse
       | A.TFloat -> let arg1 = obtem_f32 res_esq 
                   and arg2 = obtem_f32 res_dir in
                   if (match op with
	                          | A.Menor -> arg1 < arg2
	                          | A.Maior -> arg1 > arg2
	                          | A.Igual -> arg1 == arg2
	                          | A.MaiorIgual -> arg1 >= arg2
	                          | A.MenorIgual -> arg1 <= arg2
	                          | A.Diferente  -> arg1 != arg2
                              | _ -> 
                               failwith "avalia_relacional: operacao float invalida"
                           )
                   then S.ExpTrue
                   else S.ExpFalse
       | A.TString -> let arg1 = obtem_string res_esq 
                      and arg2 = obtem_string res_dir in
                      if (match op with
	                        | A.Menor -> arg1 < arg2
	                        | A.Maior -> arg1 > arg2
	                        | A.Igual -> arg1 == arg2
	                        | A.MaiorIgual -> arg1 >= arg2
	                        | A.MenorIgual -> arg1 <= arg2
	                        | A.Diferente  -> arg1 != arg2
                            | _ -> 
                             failwith "avalia_relacional: operacao string invalida"
                         )
                      then S.ExpTrue
                      else S.ExpFalse
       |  _ -> failwith "avalia_relacional: operacao invalida"
      ) 
    and avalia_logico () =
      let arg1 = obtem_bool res_esq 
      and arg2 = obtem_bool res_dir in
      if (match op with
            A.And  -> arg1 && arg2
          | A.Or -> arg2 || arg2
          | _ -> failwith "avalia_logico: operacao invalida" 
         )
      then S.ExpTrue
      else S.ExpFalse

    in
    (match (classifica op) with
               Aritmetico -> avalia_aritmetico ()
             | Relacional -> avalia_relacional ()
             | Logico -> avalia_logico ()
    )
            
  | S.ExpUna (op, exp) ->
     let res_exp = avalia_exp amb exp in
     let avalia_aritmetico () =
      (match exp.S.tinf with
         A.TInt -> let arg1 = obtem_i32 res_exp in S.ExpInt (- arg1)
       | A.TFloat -> let arg1 = obtem_f32 res_exp in S.ExpFloat (-. arg1)
       | _ -> failwith "avalia_exp unaria: erro aritmetico"
      )
     and avalia_logico () =
       let arg1 = obtem_bool res_exp in
       if (match op with
             A.Not  -> not arg1
           | _ -> failwith "avalia_exp unaria: erro logico"
          )
       then S.ExpTrue
       else S.ExpFalse
    
     in
      (match op with
         A.Not -> avalia_logico ()
       | A.MenosUm -> avalia_aritmetico ()
      ) 

  | S.ExpChamada (nome, args) ->
     let rec insere_parametros amb ps vs =
       match (ps, vs) with
         (p::ps), (v::vs) -> 
         let _ = Amb.insere_param amb p v in
         insere_parametros amb ps vs
       | [], [] -> ()
       | _ -> failwith "Numero incorreto de parametros"
     in
     ( let open A in
       let resultados_args = List.map (avalia_exp amb) args  in
       if (List.mem nome !funcoes_especiais)
       then avalia_fn_especial nome resultados_args
       else
         try
           match (Amb.busca amb nome) with 
             Amb.EntFun {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
             let nomes_formais = List.map fst fn_formais in
             let amb_fn = Amb.novo_escopo amb in
             let _ = insere_parametros amb_fn nomes_formais resultados_args in
             (try
                 avalia_cmd amb_fn fn_corpo 
               with Valor_retornado exp -> exp)

           | Amb.EntVar _ -> failwith "avalia_exp: variavel usada como funcao"
                                      
         with Not_found -> 
           failwith (Printf.sprintf "avalia_exp: a funcao %s nao foi encontrada" nome)
     )
and avalia_fn_especial nome args =
  let open Printf in 
  match nome with
    "printInt" -> (match args with
                      [a1] -> let arg1 = obtem_i32 a1 in
                              let _ = print_int arg1 in S.ExpVoid
                    | _ -> failwith 
                          (sprintf "avalia_fn_especial: %s numero errado de argumentos" nome)
                   )
  | "printString" -> (match args with
                      [a1] -> let arg1 = obtem_string a1 in
                              let _ = print_string arg1 in S.ExpVoid
                    | _ -> failwith 
                           (sprintf "avalia_fn_especial: %s numero errado de argumentos" nome)
                   )
  | "printFloat" -> (match args with
                      [a1] -> let arg1 = obtem_f32 a1 in
                              let _ = print_float arg1 in S.ExpVoid
                    | _ -> failwith 
                           (sprintf "avalia_fn_especial: %s numero errado de argumentos" nome)
                   )
  | "sqrt" -> (match args with
                      [a1] -> let arg1 = obtem_f32 a1 in
                              S.ExpFloat (sqrt arg1)
                    | _ -> failwith 
                           (sprintf "avalia_fn_especial: %s numero errado de argumentos" nome)
                   )
  | _ -> failwith (sprintf "avalia_fn_especial: %s - nao sei como interpretar" nome)

			
let interprete ast =
  let open Amb in
	let amb = novo_amb [] in
	let _ = funcoes_especiais := List.map (fun (n,ps,tr) -> n) S.fn_predefinidas in
	let _ = List.iter (insere_dec amb) ast in
  try
    match busca amb "main" with
      EntFun main -> avalia_cmd amb main.A.fn_corpo
    | EntVar _ -> failwith "main nao pode ser o nome de uma variavel"
  with
    Not_found -> failwith "intereprete: nenhuma funcao main foi encontrada"


