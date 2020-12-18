(* Errors thrown during type checking *)
exception TypeError of string * string
exception EvalError of string * string

(* Errors thrown during evaulation *)
exception RuntimeError of string * string
exception UncaughtTypeError of string * string