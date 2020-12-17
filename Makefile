# Add .ml and .mli files to MODULES w/o extension
MODULES=main pprint ast eval check
OBJECTS=$(MODULES:=.cmo)
SRC_DIR=src
BUILD_DIR=_build
MAIN=main.native
EXEC=crispy

OCAMLBUILD=ocamlbuild -use-ocamlfind -I $(SRC_DIR) -build-dir $(BUILD_DIR)

default: crispy

build:
	$(OCAMLBUILD) $(OBJECTS)

crispy: build
	$(OCAMLBUILD) $(MAIN)
	mv $(BUILD_DIR)/$(SRC_DIR)/$(MAIN) ./$(EXEC)

clean:
	ocamlbuild -clean
	rm ./crispy

test: crispy
	./run-tests.sh
