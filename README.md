
# 🥓 Crispy Programming Language

Crispy is statically-typed functional programming language with type inference. It is designed to be a simple language with *crispy* language features to make it useful.

## Core Features

1. Type Safety -  Crispy is a statically-typed language with a built in type checker. This means that any and all type errors will be caught at compile time instead of at run time. Crispy currently supports the following types: 

    - Primitives: `int`, `bool`, `str`, `unit`
    - Tuples: `t1 * ... * tn`
    - Functions: `t1 -> ... -> tn`

2. Polymorphic Types

3. Type Inferencing
```python
# infers max : number -> number -> number
max = (x,y) >> { 
  if x > y { x } else { y } 
}
```

4. Module System - Crispy uses a simple importing mechanism which allows you to easily write modular code. For example, to import the definitions from the `vectors.cpm` module, simply add the following line to the beginning of your program:
```python
import vectors
```

### Additional Features

1. Comparison chaining
```python
x = 10;
if 0 < x <= 25 { true } else { false }
```

2. Tuple Assignment
```
# assigns as you would expect
x, y, z = 1, 2, 3;
```

## `crispy` Interpreter

### Build from Source
You can build the `cripsy` interpreter using the Makefile:
- Run `make` to build the executable `cripsy` interpreter. 
- Run `make clean` to remove object files and executables.

### Run
To execute a `<file>.cpm` Crispy program, run the `crispy` interpreter with `/.crispy <file>.cpm`.

The `examples` directory contains many example Crispy programs which demonstrate different features of the language. You can execute any of these example programs with `/.crispy examples/<file>.cpm`.

The `tests` directory contains a suite of Crispy programs that are used to verify the correctness of interpreter. You can execute all of the tests with `/.crispy tests`. When applied to a directory, the interpreter finds and executes the `<directory>/main.cpm` driver program.


