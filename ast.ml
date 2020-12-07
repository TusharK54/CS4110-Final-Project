type t =
  | T_int | T_bool | T_str | T_unit
  | T_list of t
  | T_sum of t list
  | T_prod of t list
  | T_fun of t list * t

(* Variable *)
type var = string

(* Unary operators *)
type uop =
  | Negate
  | Not

(* Binary operators *)
type bop =
  | Add   (* numerical arithmetic *)
  | Sub
  | Mul
  | Div
  | Exp
  | Mod
  | Eq    (* numerical comparisons *)
  | Ne
  | Gt
  | Ge
  | Lt
  | Le
  | And   (* logical operators *)
  | Or

(* Expressions *)
type e =
  | Unit
  | Int of int            (* literals *)
  | Bool of bool
  | Str of string
  | Var of var            (* variables *)
  | Bop of bop * e * e    (* operators *)
  | Uop of uop * e
  | Seq of e * e          (* sequence *)
  | If of e * e * e       (* if expression *)
  | Assign of var * e     (* assignment *)
