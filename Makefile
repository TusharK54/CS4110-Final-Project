# Add .ml and .mli files to MODULES w/o extension
MODULES=main pprint ast eval check
OBJECTS=$(MODULES:=.cmo)
SRC_DIR=src
BUILD_DIR=_build
MAIN=main.native # must be a module
EXEC=crispy

OCAMLBUILD=ocamlbuild -use-ocamlfind -I $(SRC_DIR) -build-dir $(BUILD_DIR)

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)
	@echo Build OK
	@$(OCAMLBUILD) $(MAIN) || echo ERROR building main native in Makefile
	mv $(BUILD_DIR)/$(SRC_DIR)/$(MAIN) ./$(EXEC)

clean:
	ocamlbuild -clean
	rm ./crispy

test: build
	./run-tests.sh

env:
	eval $(opam env)