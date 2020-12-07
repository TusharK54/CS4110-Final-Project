
## Dependencies
```
opam install ounit2
```

## Build
To build the interpreter, run `make`. This should create the executable file `tk`.

## Run
to run the sample valid program `good_prog.tk`, run `./tk examples/good_prog.tk`. This should be accepted by the parser and evaluate to a valid result.

to run the sample invalid program `bad_prog.tk`, run `./tk examples/bad_prog.tk`. This should be accepted by the parser but throw an evaluation error.
