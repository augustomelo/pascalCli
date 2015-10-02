open Asadec;;
open Printf;;

let emite_prologo oc nomearq =
    fprintf oc ".assembly extern mscorlib {}\n
    .assembly '%s' {}\n
    .class public auto ansi abstract sealed beforefieldinit Test extends [mscorlib]System.Object {\n" nomearq

let emite_prologo_fn oc nomefn =
  fprintf oc ".method public static hidebysig default int32 %s ()  cil managed {\n
        .maxstack 8\n" nomefn

let emite_epilogo_fn oc nomefn =
    fprintf oc "IL_0002: ret\n
    }\n\n
    .method private static hidebysig default void Main ()  cil managed {\n
        .entrypoint\n
        .maxstack 8\n
        IL_0000: call int32 class Test::%s()\n
        IL_0005: call void class [mscorlib]System.Console::WriteLine(int32)\n
        IL_000a: ret\n
        }\n
    }" nomefn

let emite_exp exp =
  match exp with
    ConstInt n -> string_of_int n
				
let emite_cmd oc cmd =
  match cmd with
    Ret exp -> let e = emite_exp exp in
	       fprintf oc "IL_0000: ldc.i4 %s\n" e
		       
let emite_corpo_fn oc cmds =
  List.iter (emite_cmd oc) cmds

let emite_funcao oc nomefn cmds =
  emite_prologo_fn oc nomefn;
  emite_corpo_fn oc cmds;
  emite_epilogo_fn oc nomefn

(*main()*)
let gera arqsaida arqentrada nomefn cmds =
  let oc = open_out arqsaida in
  emite_prologo oc "teste";
  emite_funcao oc nomefn cmds;
  close_out oc

let gerador_da_string nomefn cmds =
  emite_prologo stdout "teste";
  emite_funcao stdout nomefn cmds;
  
