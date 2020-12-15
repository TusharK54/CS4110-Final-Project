type typ =
  | T_int | T_bool | T_str | T_unit
  | T_sum of typ list
  | T_product of typ list
  | T_fun of typ list * typ

(* Variable *)
type var = string

type arg_list = var list

(* Unary operators *)
type uop =
  | Not
  | Assert
  | AssertFail

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
type exp =
  | Unit
  | Var of var * typ option (* variable reference *)
  | Int of int              (* literals *)
  | Bool of bool
  | Str of string
  | Slice of exp * exp * exp
  | Tuple of exp list       (* tuples *)
  | Proj of exp * exp
  | Fn of var list * exp    (* function value *)
  | App of exp * exp list   (* function application *)
  | Bop of bop * exp * exp  (* operators *)
  | Uop of uop * exp
  | Seq of exp * exp        (* sequence *)
  | Assign of var * exp     (* assignment *)
  | AssignTuple of exp * exp
  | If of exp * exp * exp   (* if expression *)
