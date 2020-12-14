
# Crispy Programming Language

Crispy is statically-typed functional programming language with type inference. It is designed to be a simple language with *crispy* language features that make it useful.

## Language Features

1. Polymorphic types

2. Type checker

3. Type inferenching

4. Comparison chaining

5. Module system

6. Formatter

## Build from Source
The Crispy interpreter can be built from source using the associated Makefile.

### Dependencies
Requires ounit2

### Makefile Instructions
- Run `make` to build the executable file `tk`. 
- Run `make test` to run a suite of test Crispy programs.
- Run `make clean` to remove object files and executables.

## Run
to run the sample valid program `good_prog.tk`, run `./tk examples/good_prog.tk`. This should be accepted by the parser and evaluate to a valid result.

to run the sample invalid program `bad_prog.tk`, run `./tk examples/bad_prog.tk`. This should be accepted by the parser but throw an evaluation error.


