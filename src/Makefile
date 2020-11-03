
NULL =

SOURCES = Session.mli Service.mli Session.ml Service.ml

OCAMLC = ocamlc -w +A-20 -rectypes -thread
OCAMLOPT = ocamlopt -w +A-20 -rectypes -thread

DEST = padovani@pianeta.di.unito.it:public_html/Software/FuSe/

SYNC_FILES = $(wildcard html/*)

all: FuSe.cma FuSe.cmxa

FuSe.cma: $(SOURCES)
	$(OCAMLC) -a -o $@ $^

FuSe.cmxa: $(SOURCES)
	$(OCAMLOPT) -a -o $@ $^

sync: $(SYNC_FILES)
	scp $^ $(DEST)

clean:
	rm -f *~ *.cma *.cmxa *.cmo *.cmi *.cmx *.o *.t *.a

.PHONY: clean html

