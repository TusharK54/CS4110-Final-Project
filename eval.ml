open Ast

type value =
  | V_unit
  | V_int of int
  | V_bool of bool
  | V_str of string
  | V_tuple of value list
  | Closure of  (var list * exp) * store
and store = (var * value) list

exception UnboundVariable of var
exception TypeError of string
exception RuntimeError of string

(* `get_var s x` returns s(x) or UnboundVariable if x is not defined on s. *)
let rec get_var (s: store) (x: var) : value =
  match s with
  | [] ->
    raise (UnboundVariable x)
  | (a,b)::z ->
    if a = x then b
    else (get_var z x)

(* `set_var (s, x, v)` returns the store s[x->v]. *)
let rec set_var (s: store) (x: var) (v: value) : store =
  match s with
  | [] ->
    [(x,v)]
  | (a,b)::z ->
    if a = x then (x,v)::z
    else (a,b)::(set_var z x v)

(* `set_var (s, xvs)` returns the store s[x->v] for each (x,v) in xvs. *)
let rec set_vars (s: store) (xvs: (var * value) list) : store =
  match xvs with
  | [] ->
    s
  | (x,v)::y ->
    set_vars (set_var s x v) y

(* `eval e` evaluates the expression e and returns it as an Ast.e type. *)
let rec eval (e: exp) : exp =
  let v, s' = eval_expr e [] in
  ast_e v

(* `ast_e v` returns a value of the corresponding Ast.e type *)
and ast_e (v: value) : exp =
  match v with
  | V_unit ->
    Unit
  | V_int i ->
    Int i
  | V_bool b ->
    Bool b
  | V_str t ->
    Str t
  | V_tuple es ->
    Tuple (List.map (fun i -> ast_e i) es)

and eval_expr (e: exp) (s: store) : (value * store) =
  match e with
  | Unit ->
    V_unit, s
  | Int n ->
    V_int n, s
  | Bool b ->
    V_bool b, s
  | Str t ->
    V_str t, s
  | Slice (e1, e2, e3) ->
    begin
      let v1, s1 = eval_expr e1 s in
      let v2, s2 = eval_expr e2 s1 in
      let v3, s3 = eval_expr e3 s2 in
      match v1 with
      | V_str t ->
        begin
          let get_n (v: value) default =
            match v with
            | V_int n -> if n < 0 then String.length t + n else n
            | V_unit -> default
            | _ -> raise (RuntimeError ("Malformed slicing: " ^ (Pprint.string_of_e e)))
          in
          let n1' = get_n v2 0 in
          let n2' = get_n v3 (String.length t) in
          if n1' <= n2' then V_str (String.sub t n1' (n2'-n1')), s3
          else raise (TypeError "String slicing out of bounds")
        end
      | _ -> raise (RuntimeError ("Malformed slicing: " ^ (Pprint.string_of_e e)))
    end
  | Tuple es ->
    let vs = List.map (fun i -> fst (eval_expr i s)) es in
    V_tuple vs, s
  | Proj (e1, e2) ->
    begin
      let v1, s1 = eval_expr e1 s in
      let v2, s2 = eval_expr e2 s1 in
      match v1, v2 with
      | V_str t, V_int n ->
        let n' = (if n < 0 then String.length t + n else n) in
        if n' < String.length t then V_str (String.sub t n' 1), s
        else raise (RuntimeError ("String index out of bounds: " ^ (Pprint.string_of_e e)))
      | V_tuple vs, V_int n ->
        begin
          match List.nth_opt vs n with
          | Some v -> v, s2
          | None -> raise (RuntimeError ("Tuple index out of bounds: " ^ (Pprint.string_of_e e)))
        end
      | _ -> raise (RuntimeError ("Malformed indexing: " ^ (Pprint.string_of_e e)))
    end
  | Var (x, _) ->
    get_var s x, s
  | Fn (xs, e1) ->

    Closure ((xs, e1), s), s
  | App (af, ax) ->
    begin
      let f, s1 = eval_expr af s in
      match f with
      | Closure ((cx, ce), cs) ->
        begin
          if List.length cx > List.length ax then
          raise (RuntimeError ("Function applied to too few arguments: " ^ (Pprint.string_of_e e)))
          else if List.length cx < List.length ax then
          raise (RuntimeError ("Function applied to too many arguments: " ^ (Pprint.string_of_e e)))
          else
          let rec eval_args (acc: value list) (sa: store) (args: exp list) : store =
            match args with
            | [] ->
              set_vars cs (List.combine cx (List.rev acc))
            | arg::z ->
              let vx, sx = eval_expr arg sa in
              eval_args (vx::acc) sx z
          in eval_expr ce (eval_args [] s1 ax)
        end
      | _ -> raise (RuntimeError ("Malformed function application: " ^ (Pprint.string_of_e e)))
    end
  | Bop (op, e1, e2) ->
    begin
      let v1, s1 = eval_expr e1 s in
      let v2, s2 = eval_expr e2 s1 in
      match op, v1, v2 with
      | Add, V_int n1, V_int n2 ->
        V_int (n1 + n2), s2
      | Add, V_str t1, V_str t2 ->
        V_str (t1^t2), s2
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
      | Eq, V_unit, V_unit ->
        V_bool true, s2
      | Eq, V_int n1, V_int n2 ->
        V_bool (n1 = n2), s2
      | Eq, V_bool b1, V_bool b2 ->
        V_bool (b1 = b2), s2
      | Eq, V_str t1, V_str t2 ->
        V_bool (t1 = t2), s2
      | Eq, V_tuple vs1, V_tuple vs2 ->
        V_bool (vs1 = vs2), s2
      | Ne, _, _ ->                       (* (x != y) >> !(x == y) *)
        eval_expr (Uop(Not, Bop(Eq, ast_e v1, ast_e v2))) s2
      | Gt, V_int n1, V_int n2 ->
        V_bool (n1 > n2), s2
      | Ge, V_int n1, V_int n2 ->
        V_bool (n1 >= n2), s2
      | Lt, V_int n1, V_int n2 ->
        V_bool (n1 < n2), s2
      | Le, V_int n1, V_int n2 ->
        V_bool (n1 <= n2), s2
      | And, V_bool b1, _ when not b1 ->  (* short-circuit evaluation *)
        V_bool false, s2
      | And, V_bool b1, V_bool b2 when b1 ->
        V_bool b2, s2
      | Or, V_bool b1, _ when b1 ->       (* short-circuit evaluation *)
        V_bool true, s2
      | Or, V_bool b1, V_bool b2 when not b1 ->
        V_bool b2, s2
      | _, _, _ ->
        raise (RuntimeError ("Malformed binary operator: " ^ (Pprint.string_of_e e)))
    end
  | Uop (op, e1) ->
    begin
      let v1, s1 = eval_expr e1 s in
      match op, v1 with
      | Not, V_bool b ->
        V_bool(not b), s1
      | Assert, V_bool b ->
        if b then (V_unit, s1)
        else raise (RuntimeError ("False assertion: " ^ (Pprint.string_of_e e)))
      | Assert, _ ->
        raise (RuntimeError ("Invalid assertion: " ^ (Pprint.string_of_e e)))
      | AssertFail, _ ->
        failwith "Unimlemented AssertFail"
      | _, _ ->
        raise (RuntimeError ("Malformed unary operator: " ^ (Pprint.string_of_e e)))
    end
  | Seq (e1, e2) ->
    let _, s1 = eval_expr e1 s in
    eval_expr e2 s1
  | Assign (x, e) ->
    let v1, s1 = eval_expr e s in
    V_unit, set_var s1 x v1
  | AssignTuple (xs, es) ->
    failwith "Unimplemented AssignTuple"
  | If (g, e1, e2) ->
    begin
      let v1, s1 = eval_expr g s in
      match v1 with
      | V_bool b ->
      if b then eval_expr e1 s1 else eval_expr e2 s1
      | _ ->
      raise (TypeError "If expression guard must be a bool")
    end
