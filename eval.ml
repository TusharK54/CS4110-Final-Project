open Ast

type value =
  | V_unit
  | V_int of int
  | V_bool of bool
  | V_str of string
and store = (var * value) list

exception UnboundVariable of var
exception TypeError of string

(* `set_var (s, x, v)` returns the store s[x->v]. *)
let rec set_var s x v : store =
  match s with
  | [] ->
    [(x,v)]
  | (a,b)::z ->
    if a = x then (x,v)::z
    else (a,b)::(set_var z x v)

(* `get_var s x` returns s(x) or UnboundVariable if x is not defined on s. *)
let rec get_var s x : value =
  match s with
  | [] ->
    raise (UnboundVariable x)
  | (a,b)::z ->
    if a = x then b
    else (get_var z x)

(* `eval e` evaluates the expression e and returns it as an Ast.e type. *)
let rec eval e =
  let v, s' = eval_expr e [] in
  match v with
  | V_unit -> Unit
  | V_int i -> Int i
  | V_bool b -> Bool b
  | V_str t -> Str t

and eval_expr e (s: store) : (value * store) =
  match e with
  | Unit ->
    V_unit, s
  | Int n ->
    V_int n, s
  | Bool b ->
    V_bool b, s
  | Str t ->
    V_str t, s
  | Var x ->
    get_var s x, s
  | Bop (op, e1, e2) ->
    begin
      let v1, s1 = eval_expr e1 s in
      let v2, s2 = eval_expr e2 s1 in
      match op, v1, v2 with
      | Add, V_int n1, V_int n2 ->
        V_int (n1 + n2), s2
      | Sub, V_int n1, V_int n2 ->
        V_int (n1 - n2), s2
      | Mul, V_int n1, V_int n2 ->
        V_int (n1 * n2), s2
      | Div, V_int n1, V_int n2 ->
        V_int (n1 / n2), s2
      | Exp, V_int n1, V_int n2 ->
        V_unit, s2
      | Mod, V_int n1, V_int n2 ->
        V_int (n1 mod n2), s2
      | Eq, V_int n1, V_int n2 ->
        V_bool (n1 = n2), s2
      | Ne, V_int n1, V_int n2 ->
        V_bool (n1 <> n2), s2
      | Gt, V_int n1, V_int n2 ->
        V_bool (n1 > n2), s2
      | Ge, V_int n1, V_int n2 ->
        V_bool (n1 >= n2), s2
      | Lt, V_int n1, V_int n2 ->
        V_bool (n1 < n2), s2
      | Le, V_int n1, V_int n2 ->
        V_bool (n1 <= n2), s2
      | And, V_bool b1, V_bool b2 ->
        V_bool (b1 && b2), s2
      | Or, V_bool b1, V_bool b2 ->
        V_bool (b1 || b2), s2
      | _, _, _ ->
        raise (TypeError "Malformed binary operator")
    end
  | Uop (op, e) ->
    begin
      let v1, s1 = eval_expr e s in
      match op, v1 with
      | Negate, V_int n ->
        V_int (0 - n), s1
      | Not, V_bool b ->
        V_bool(not b), s1
      | _, _ ->
        raise (TypeError "Malformed unary operator")
    end
  | Seq (e1, e2) ->
    let _, s1 = eval_expr e1 s in
    eval_expr e2 s1
  | Assign (x, e) ->
    let v1, s1 = eval_expr e s in
    v1, set_var s1 x v1
  | If (g, e1, e2) ->
    begin
      let v1, s1 = eval_expr g s in
      match v1 with
      | V_bool b ->
      if b then eval_expr e1 s1 else eval_expr e2 s1
      | _ ->
      raise (TypeError "If expression guard must be a bool")
    end
