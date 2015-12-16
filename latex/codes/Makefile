CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc

compilador: vanguarda retaguarda

vanguarda: sintatico.cmo imprime.cmo lexico.cmo tabsimb.cmo ambiente.cmo semantico.cmo

interprete: vanguarda ambInterpretador.cmi ambInterpretador.cmo interprete.cmo

retaguarda: tradutorRI.cmo linearizador.cmo seleciona.cmo montador.cmo

seleciona.cmo: asabs.cmo arvoreRI.cmo montador.cmo temp.cmo
	$(CAMLC) -c seleciona.ml

linearizador.cmo: arvoreRI.cmo rotulo.cmo linearizador.ml
	$(CAMLC) -c linearizador.ml

tradutorRI.cmo: asadec.cmo arvoreRI.cmo rotulo.cmo temp.cmo tradutorRI.ml
	$(CAMLC) -c tradutorRI.ml

asadec.cmi: asabs.cmo asadec.ml
	$(CAMLC) -c asadec.ml

ambiente.cmo: tabsimb.cmi asabs.cmi asabs.cmo ambiente.cmi ambiente.ml
	$(CAMLC) -c ambiente.ml

tabsimb.cmo:  asabs.cmo tabsimb.cmi tabsimb.ml
	$(CAMLC) -c tabsimb.ml

semantico.cmo: ambiente.cmi asabs.cmo semantico.cmi semantico.ml
	$(CAMLC) -c semantico.ml

sintatico.cmo: asabs.cmo sintatico.cmi sintatico.ml
	$(CAMLC) -c sintatico.ml

sintatico.cmi: asabs.cmo sintatico.mli
	$(CAMLC) -c sintatico.mli

sintatico.ml sintatico.mli: sintatico.mly
	$(CAMLYACC) -v sintatico.mly

lexico.ml: sintatico.cmi imprime.cmi lexico.mll
	$(CAMLLEX) lexico.mll

imprime.cmi: sintatico.cmi sintatico.cmo imprime.mli
	$(CAMLC) -c imprime.mli

imprime.cmo: sintatico.cmi imprime.cmi imprime.ml
	$(CAMLC) -c imprime.ml

# Regras comuns
.SUFFIXES: .ml .mli .cmo .cmi 
.ml.cmo: 
	$(CAMLC) -c $<

.mli.cmi: 
	$(CAMLC) -c $<

clean:
	rm -f lexico.ml sintatico.ml sintatico.mli sintatico.output
	rm -f *.cm[iox]
