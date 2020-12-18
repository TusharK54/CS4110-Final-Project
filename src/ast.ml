type typ =
  | T_any
  | T_nop
  | T_unit | T_int | T_bool | T_str
  | T_sum of typ list
  | T_product of typ list
  | T_fun of typ list * typ

(* Variable *)
type var = string

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
  | Var of var              (* variable reference *)
  | Nop                     (* special purpose *)
  | Unit                    (* unit *)
  | Int of int              (* literals *)
  | Bool of bool
  | Str of string
  | Tuple of exp list       (* tuples *)
  | Slice of exp * exp * exp
  | Proj of exp * exp
  | Fn of arg_list * exp    (* function value *)
  | App of exp * exp list   (* function application *)
  | Bop of bop * exp * exp  (* operators *)
  | Uop of uop * exp
  | Seq of exp * exp        (* sequence *)
  | Assign of var * exp     (* assignment *)
  | AssignTuple of var list * exp list
  | If of exp * exp * exp   (* if expression *)

and arg_list = (var * typ) list