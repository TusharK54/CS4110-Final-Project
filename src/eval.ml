open Ast
open Errors

let raise_run_exn e msg =
  raise (RuntimeError (msg, (Pprint.string_of_e e)))

let raise_typ_exn e msg =
  raise (UncaughtTypeError (msg, (Pprint.string_of_e e)))

type value =
  | V_nop
  | V_unit
  | V_int of int
  | V_bool of bool
  | V_str of string
  | V_tuple of value list
  | Closure of ((var * typ) list * exp) * store
and store = (var * value) list

(* `e_of_value v` takes a value `v` and returns the corresponding Ast.e type *)
let rec e_of_value (v: value) : exp =
  match v with
  | V_nop ->
    Nop
  | V_unit ->
    Unit
  | V_int i ->
    Int i
  | V_bool b ->
    Bool b
  | V_str t ->
    Str t
  | V_tuple es ->
    Tuple (List.map (fun i -> e_of_value i) es)
  | Closure ((xts, e1), s) ->
    Fn (xts, e1)

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

(* `eval e` returns the evaluated form of the expression `e`. *)
let rec eval (e: exp) : exp =
  let v, s = eval_exp e [] in e_of_value v

and eval_exp (e: exp) (s: store) : value * store =
  let raise_run_exn msg = raise_run_exn e msg in
  let raise_typ_exn msg = raise_typ_exn e msg in

  match e with
  | Var x ->
    begin
    (* returns s(x) or raises exception if x is not defined on s. *)
    let rec get_var (s: store) (x: var) : value =
      match s with
      | [] ->
        raise_run_exn ("Unbound variable \""^x^"\"")
      | (a,b)::z ->
        if a = x then b
        else (get_var z x)
    in get_var s x, s
  end
  | Nop ->
    V_nop, s
  | Unit ->
    V_unit, s
  | Int n ->
    V_int n, s
  | Bool b ->
    V_bool b, s
  | Str t ->
    V_str t, s
  | Tuple es ->
    let vs = List.map (fun i -> fst (eval_exp i s)) es in
    V_tuple vs, s
  | Slice (e1, e2, e3) ->
    begin
      let v1, s1 = eval_exp e1 s in
      let v2, s2 = eval_exp e2 s1 in
      let v3, s3 = eval_exp e3 s2 in
      match v1 with
      | V_str t ->
        begin
          let get_n (v: value) default =
            match v with
            | V_int n -> 
              if n >= 0 then n
              else String.length t + n
            | V_nop -> 
              default
            | _ -> 
              raise_typ_exn "String slicing arguments"
          in
          let n1 = get_n v2 0 in
          let n2 = get_n v3 (String.length t) in
          if n1 <= n2 then 
            V_str (String.sub t n1 (n2-n1)), s3
          else 
            raise_run_exn "String slicing out of bounds"
        end
      | _ -> 
        raise_typ_exn "Slicing"
    end
  | Proj (e1, e2) ->
    begin
      let v1, s1 = eval_exp e1 s in
      let v2, s2 = eval_exp e2 s1 in
      match v1, v2 with
      | V_str t, V_int n ->
        let n' = (if n < 0 then String.length t + n else n) in
        if n' < String.length t then 
          V_str (String.sub t n' 1), s
        else
          raise_run_exn "String index out of bounds"
      | V_tuple vs, V_int n ->
        begin
          match List.nth_opt vs n with
          | Some v -> 
            v, s2
          | None -> 
            raise_run_exn "Tuple index out of bounds"
        end
      | _ -> 
        raise_typ_exn "Indexing"
    end
  | Fn (xts, e1) ->
    Closure ((xts, e1), s), s
  | App (e1, es) ->
    begin
      let v1, s1 = eval_exp e1 s in
      match v1 with
      | Closure ((xts, e2), s2) ->
        begin
          let xs, ts = List.split xts in
          if List.length xs > List.length es then
            raise_run_exn "Function applied to too few arguments"
          else if List.length xs < List.length es then
            raise_run_exn "Function applied to too many arguments"
          else
            let rec eval_args (acc: value list) (args: exp list) : store =
              match args with
              | [] ->
                set_vars s2 (List.combine xs (List.rev acc))
              | arg::z ->
                let vx, _ = eval_exp arg s1 in
                eval_args (vx::acc) z
            in fst(eval_exp e2 (eval_args [] es)), s1
        end
      | _ -> 
        raise_typ_exn "Function application"
    end
  | Bop (op, e1, e2) ->
    begin
      let v1, s1 = eval_exp e1 s in
      let v2, s2 = eval_exp e2 s1 in
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
        eval_exp (Uop(Not, Bop(Eq, e_of_value v1, e_of_value v2))) s2
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
        raise_typ_exn "Binary operator"
    end
  | Uop (op, e1) ->
    begin
      let v1, s1 = eval_exp e1 s in
      match op, v1 with
      | Not, V_bool b ->
        V_bool(not b), s1
      | Assert, V_bool b ->
        if b then 
          V_unit, s1
        else 
          raise_run_exn "False assertion"
      | AssertFail, _ ->
        failwith "Unimlemented AssertFail"
      | _, _ ->
        raise_typ_exn "Unary operator"
    end
  | Seq (e1, e2) ->
    let _, s1 = eval_exp e1 s in
    eval_exp e2 s1
  | Assign (x, e1) ->
    begin
      let v1, s1 = eval_exp e1 s in
      match v1 with
      | Closure ((cx, ce), cs) ->
        let s2 = set_var cs x v1 in
        let v2 = Closure((cx, ce), s2) in
        V_unit, set_var s1 x v2
      | _ ->
        V_unit, set_var s1 x v1
    end
  | AssignTuple (xs, es) ->
    if List.length xs <> List.length es then
      raise_run_exn "Assignment between tuples of unequal length"
    else
      let vs = List.map (fun i -> fst (eval_exp i s)) es in
      let zip = List.combine xs vs in
      V_unit, set_vars s zip
  | If (e1, e2, e3) ->
    begin
      let v1, s1 = eval_exp e1 s in
      match v1 with
      | V_bool b ->
        if b then 
          eval_exp e1 s1 
        else 
          eval_exp e2 s1
      | _ ->
        raise_typ_exn "If-expression guard"
    end
