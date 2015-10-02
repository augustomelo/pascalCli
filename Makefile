CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc

compilador: vanguarda retaguarda

retaguarda: gerador.cmo

vanguarda: semantico.cmo sintatico.cmo lexico.cmo

compsint: sintatico.cmo lexico.cmo

gerador.cmo: gerador.ml asadec.cmi asabs.cmi tabsimb.cmo
	$(CAMLC) -c gerador.ml

semantico.cmo: semantico.ml asadec.cmi asabs.cmi tabsimb.cmo
	$(CAMLC) -c semantico.ml

sintatico.cmo: asabs.cmi sintatico.cmi sintatico.ml 
	$(CAMLC) -c sintatico.ml

lexico.cmo: lexico.ml
	$(CAMLC) -c lexico.ml

tabsimb.cmo:  asadec.cmi asabs.cmi tabsimb.ml
	$(CAMLC) -c tabsimb.ml

asadec.cmi: asabs.cmi asadec.ml
	$(CAMLC) -c asadec.ml

asabs.cmi: asabs.ml
	$(CAMLC) -c asabs.ml

sintatico.ml: sintatico.mly
	$(CAMLYACC) -v sintatico.mly

lexico.ml: sintatico.cmi lexico.mll
	$(CAMLLEX) lexico.mll

sintatico.cmi: sintatico.mli
	$(CAMLC) -c sintatico.mli

sintatico.mli: sintatico.mly
	$(CAMLYACC) -v sintatico.mly

clean:
	rm -f *.cmo *.cmi *.*~ lexico.ml sintatico.ml sintatico.mli sintatico.output

