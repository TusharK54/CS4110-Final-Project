type typ =
  | T_any
  | T_nop
  | T_unit 
  | T_int | T_bool | T_str
  | T_product of typ list
  | T_sum of typ list
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

let rec compare_typ t1 t2 =
  match t1, t2 with
  | T_nop, T_nop ->
    true
  | T_unit, T_unit ->
    true
  | T_int, T_int -> 
    true
  | T_bool, T_bool -> 
    true
  | T_str, T_str -> 
    true
  | T_product ts1, T_product ts2 ->
    compare_typ_lists ts1 ts2
  | T_fun (ts1, r1), T_fun (ts2, r2) ->
    (compare_typ r1 r2) && (compare_typ_lists ts1 ts2)
  | _ -> 
    false

and compare_typ_lists ts1 ts2 =
  if List.length ts1 <> List.length ts2 then 
    false
  else 
    let tts = List.combine ts1 ts2 in
    List.fold_left (fun b (t1, t2) -> (b && (compare_typ t1 t2))) true tts