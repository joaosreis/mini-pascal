#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

# - we are using menhir, the modern replacement for OCamlYacc
# OCB_FLAGS = -use-ocamlfind             -I src -I lib # uses ocamlyacc
OCB_FLAGS = -use-ocamlfind -use-menhir -menhir "menhir --explain" -I src -I lib # uses menhir

OCB = ocamlbuild $(OCB_FLAGS)

BIN = main.native

all: 		native byte # profile debug

clean:		clean_tests
			$(OCB) -clean

native:  	sanity
			$(OCB) main.native

byte: 		sanity
			$(OCB) main.byte

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
			# check that menhir is installed, use "opam install menhir"
			which menhir

tests:		native clean_tests
	@for f in tests/*.pas; do \
	echo "===== testing $$f ============"; \
	./$(BIN) $$f && gcc tests/`basename $$f .pas`.s && ./a.out; done

clean_tests:
	rm -f tests/*.s
	rm -f a.out

.PHONY: 	all clean byte native profile debug sanity test
