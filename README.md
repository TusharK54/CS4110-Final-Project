
## Dependencies
```
opam install ounit2
```

## Build
To build the interpreter, run `make`. This should create the executable file `tk`.

## Run
to run the sample valid program `good_prog.tk`, run `./tk good_prog.tk`. This should immediately return without printing anything.

to run the sample invalid program `bad_prog.tk`, run `./tk bad_prog.tk`. This should throw a syntax error.
