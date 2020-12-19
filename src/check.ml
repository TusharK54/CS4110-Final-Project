open Ast
open Errors
open Eval

let raise_run_exn e msg =
  raise (EvalError (msg, (Pprint.string_of_e e)))

let raise_typ_exn e msg =
  raise (TypeError (msg, (Pprint.string_of_e e)))

type ctx = (var * typ) list

(* `set_typ (c, x, t)` returns the context c[x->t]. *)
let rec set_typ (c: ctx) (x: var) (t: typ) : ctx =
  match c with
  | [] ->
    [(x,t)]
  | (a,b)::z ->
    if a = x then (x,t)::z
    else (a,b)::(set_typ z x t)

(* `set_typs (c, xts)` returns the context c[x->t] for each (x,t) in xts. *)
let rec set_typs (c: ctx) (xts: (var * typ) list) : ctx =
  match xts with
  | [] ->
    c
  | (v,t)::z ->
    set_typs (set_typ c v t) z

(* `check e` returns the type of the expression `e`. *)
let rec check (e: exp) : typ =
  let t, c = check_exp e [] in t

and check_exp (e: exp) (c: ctx) : typ * ctx =
  let raise_run_exn msg = raise_run_exn e msg in
  let raise_typ_exn msg = raise_typ_exn e msg in

  match e with
  | Var x ->
    begin
    (* `get_typ c x` returns c(x) or TypeError if x is not defined on c. *)
    let rec get_typ (c: ctx) (x: var) : typ =
      match c with
      | [] ->
        raise_typ_exn ("Untyped variable \""^x^"\"") (* T_any? *)
      | (a,b)::z ->
        if a = x then b
        else (get_typ z x)
    in get_typ c x, c
    end
  | Nop ->
    T_nop, c
  | Unit ->
    T_unit, c
  | Int n ->
    T_int, c
  | Bool b ->
    T_bool, c
  | Str s ->
    T_str, c
  | Tuple es ->
    let ts = List.map (fun e -> fst (check_exp e c)) es in
    T_product ts, c
  | Slice (e1, e2, e3) ->
    begin
      let t1, c1 = check_exp e1 c in
      let t2, c2 = check_exp e2 c1 in
      let t3, c3 = check_exp e3 c2 in
      if not ((compare_typ t2 T_int || compare_typ t2 T_nop) &&
        (compare_typ t3 T_int || compare_typ t3 T_nop)) then
        raise_typ_exn ""
      else
        match t1 with
        | T_str ->
          T_str, c3
        | _ ->
          raise_typ_exn ""
    end
  | Proj (e1, e2) ->
    begin
      let t1, c1 = check_exp e1 c in
      let t2, c2 = check_exp e2 c1 in
      if not (compare_typ t2 T_int) then
        raise_typ_exn "Projection index must be an int"
      else
        match t1 with
        | T_str ->
          T_str, c2
        | T_product ts ->
          begin
            try
              let n = 
                match eval e2 with
                | Int n -> n
                | _ -> raise_run_exn "" (* unreachable *)
              in
              match List.nth_opt ts n with
              | Some t2 ->
                t2, c2
              | None ->
                raise_run_exn "Tuple index out of bounds"
            with
              | Errors.RuntimeError (_,_) ->
                raise_run_exn "Tuple projection cannot contain a variable reference"
          end
        | _ ->
          raise_typ_exn ""
    end
  | Fn (xts, e1) ->
    let xs, ts = List.split xts in
    let c1 = set_typs c xts in
    let t1, c2 = check_exp e1 c1 in
    T_fun (ts, t1), c
  | App (e1, es) ->
    begin
      let t1, c1 = check_exp e1 c in
      match t1 with
      | T_fun (ts, tr) ->
        if List.length ts > List.length es then
          raise_typ_exn "Function applied to too few arguments" (* currying? *)
        else if List.length ts < List.length es then
          raise_typ_exn "Function applied to too many arguments"
        else
          let ts' = List.map (fun e -> fst (check_exp e c)) es in
          let tts = List.combine ts ts' in
          let f_tts = List.filter (fun (a, b) -> not (compare_typ a b)) tts in

          if List.length f_tts > 0 then
            raise_typ_exn "Function arguments have incompatible types"
          else
            tr, c1
      | _ ->
        raise_typ_exn "Cannot apply a non-function"
    end
  | Bop (op, e1, e2) ->
    begin
      let t1, c1 = check_exp e1 c in
      let t2, c2 = check_exp e2 c1 in
      match op with
      | Add ->
        begin
          if t1 <> t2 then 
            raise_typ_exn ""
          else 
            match t2 with
            | T_int | T_str -> 
              t2, c2
            | _ -> 
              raise_typ_exn ""
        end
      | Sub | Mul | Div | Exp | Mod ->
        if compare_typ t1 t2 && compare_typ t1 T_int then T_int, c2
        else raise_typ_exn ""
      | Gt | Lt | Ge | Le ->
        if compare_typ t1 t2 && compare_typ t1 T_int then T_bool, c2
        else raise_typ_exn ""
      | And | Or ->
        if compare_typ t1 t2 && compare_typ t1 T_bool then T_bool, c2
        else raise_typ_exn ""      
      | Eq | Ne ->
        begin
          if not (compare_typ t1 t2) then 
            raise_typ_exn ""
          else
            match t2 with
            | T_unit | T_int | T_bool | T_str | T_product _ ->
              T_bool, c2
            | __-> 
              raise_typ_exn ""
        end
    end
  | Uop (op, e1) ->
    begin
      let t1, c1 = check_exp e1 c in
      match op with 
      | Not ->
        if compare_typ t1 T_bool then T_bool, c1
        else raise_typ_exn ""
      | Assert ->
        if compare_typ t1 T_bool then T_unit, c1
        else raise_typ_exn ""
      | AssertFail -> T_unit, c1
    end
  | Seq (e1, e2) ->
    let t1, c1 = check_exp e1 c in 
    check_exp e2 c1
  | Assign (x, e1) ->
    let t1, c1 = check_exp e1 c in 
    T_unit, set_typ c1 x t1
  | AssignTuple (xs, es) ->
    let ts = List.map (fun e -> fst (check_exp e c)) es in
    T_unit, set_typs c (List.combine xs ts)
  | If (e1, e2, e3) ->
    begin
      let t1, c1 = check_exp e1 c in
      let t2, c2 = check_exp e2 c1 in
      let t3, c3 = check_exp e3 c2 in
      match t1 with
      | T_bool ->
        if compare_typ t2 t3 then t3, c3
        else raise_typ_exn ""
      | _ ->
        raise_typ_exn ""
    end