NULL =

TESTS = \
  Tmath \
  Tpapermath \
  TpapermathMonad \
  Tadder \
  Tchoices \
  Tcounter \
  Toverlap \
  Tsubtyping \
  Tunused \
  Tweird \
  Tlist \
  Ttree \
  Tast \
  Tlock \
  Techo \
  TrecursiveEcho \
  $(NULL)

TESTS_OPT = $(TESTS:%=%.opt)

OCAMLC = ocamlc -I ../src -w +A -rectypes -thread unix.cma threads.cma
OCAMLOPT = ocamlopt -I ../src -w +A -rectypes -thread unix.cmxa threads.cmxa
DECODER = ../decoder/rosetta

all: $(TESTS) $(TESTS_OPT)

all_t: $(TESTS:%=%.t)

all_st: $(TESTS:%=%.st)

%.t: %.ml
	@$(OCAMLC) -i $<

%.st: %.ml
	@$(OCAMLC) -i $< | $(DECODER)

%: %.ml ../src/FuSe.cma
	$(OCAMLC) -o $@ FuSe.cma $<

%.opt: %.ml ../src/FuSe.cmxa
	$(OCAMLOPT) -o $@ FuSe.cmxa $<

clean:
	rm -f *~ *.cmo *.cmi *.cmx *.o *.t $(TESTS) $(TESTS_OPT)

rename: $(TESTS:test-%=test.%.ml) $(FAILING_TESTS:fail-%=fail.%.ml)

test.%: test-%
	svn mv $< $@

fail.%: fail-%
	svn mv $< $@

.PHONY: clean html

