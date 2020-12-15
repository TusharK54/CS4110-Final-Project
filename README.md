
# 🥓 `crispy` Interpreter

`crispy` is statically-typed functional programming language with type inference. It is designed to be a simple language with *crispy* language features to make it useful.

## Core Features

1. Type Safety

2. Polymorphic Types

3. Type Inferencing
```
# infers max : number >> number >> number
max = (x,y) >> { if x > y { x } else { y } }
```

4. Module System

### Additional Features

1. Comparison chaining
```
x = 10;
if 0 < x <= 25 { true } else { false }
```

2. Tuple Assignment
```
# assigns as you would expect
x, y, z = 1, 2, 3;
```

## Build from Source
You can build the `cripsy` interpreter using the Makefile:
- Run `make` to build the executable file `tk`. 
- Run `make test` to run a suite of test Crispy programs.
- Run `make clean` to remove object files and executables.

### Run
To execute a `.cpm` program file, run the `crispy` interpreter with `/.crispy <file>.cpm`.

The `examples` directory contains many example Crispy programs, to show different features of the language. You can execute any of these example programs with `/.crispy examples/<file>.cpm`.

The `tests` directory contains a suite of Crispy programs that are used to verify the correctness of interpreter. You can execute all of the tests with `/.crispy tests`. When applied to a directory, the interpreter finds and executes the `<directory>/main.cpm` driver program.


