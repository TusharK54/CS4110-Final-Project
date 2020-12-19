open Ast

let rec string_of_e (e: exp) : string =
  match e with
  | Var x ->
    x
  | Nop ->
    ""
  | Unit ->
    "()"
  | Int n ->
    let s = string_of_int n in
    if n < 0 then String.concat "" ["("; s; ")"]
    else s
  | Bool b ->
    if b then "true" else "false"
  | Str s ->
    String.concat "" ["\""; s; "\""]
  | Tuple es ->
    "(" ^ (String.concat ", " (List.map (fun i -> string_of_e i) es)) ^ ")"
  | Slice (e1, n1, n2) ->
    let s = string_of_e e1 in
    let s1 = string_of_e n1 in
    let s2 = string_of_e n2 in
    String.concat "" [s; "["; s1; ":"; s2; "]"]
  | Proj (t, n) ->
    let s1 = string_of_e t in
    let s2 = string_of_e n in
    String.concat "" [s1; "["; s2; "]"]
  | Fn (xts, e1) ->
    let s1 = xts |> List.map (fun (x,t)-> x^":"^(string_of_t t)) |> String.concat ", " in
    let s2 = string_of_e e1 in
    String.concat "" ["("; s1 ;") {\n"; s2 ; "\n}\n"]
  | App (ef, es) ->
    let f = string_of_e ef in
    let args = List.map (fun i -> string_of_e i) es |> String.concat ", " in
    String.concat "" [f; "("; args; ")"]
  | Bop (op, e1, e2) ->
    begin
      let s1 = string_of_e e1 in
      let s2 = string_of_e e2 in
      match op with
      | Add ->
        String.concat "" ["("; s1; " + "; s2; ")"]
      | Sub ->
        String.concat "" ["("; s1; " - "; s2; ")"]
      | Mul ->
        String.concat "" ["("; s1; " * "; s2; ")"]
      | Div ->
        String.concat "" ["("; s1; " / "; s2; ")"]
      | Exp ->
        String.concat "" ["("; s1; " ^ "; s2; ")"]
      | Mod ->
        String.concat "" ["("; s1; " % "; s2; ")"]
      | Eq ->
        String.concat "" ["("; s1; " == "; s2; ")"]
      | Ne ->
        String.concat "" ["("; s1; " != "; s2; ")"]
      | Gt ->
        String.concat "" ["("; s1; " > "; s2; ")"]
      | Ge ->
        String.concat "" ["("; s1; " >= "; s2; ")"]
      | Lt ->
        String.concat "" ["("; s1; " < "; s2; ")"]
      | Le ->
        String.concat "" ["("; s1; " <= "; s2; ")"]
      | And ->
        String.concat "" ["("; s1; " && "; s2; ")"]
      | Or ->
        String.concat "" ["("; s1; " || "; s2; ")"]
    end
  | Uop (op, e) ->
    begin
      let s = string_of_e e in
      match op with
      | Not ->
      String.concat "" ["(!"; s; ")"]
      | Assert ->
      String.concat "" ["assert "; s]
      | AssertFail ->
      String.concat "" ["assertfail "; s]
    end
  | Seq (e1, e2) ->
    let s1 = string_of_e e1 in
    let s2 = string_of_e e2 in
    String.concat "" [s1; ";\n"; s2]
  | Assign (x, e) ->
    let s = string_of_e e in
    String.concat " " [x; "="; s]
  | AssignTuple (xs, es) ->
    let s1 = String.concat ", " xs in
    let s2 = String.concat ", " (List.map (fun i -> string_of_e i) es) in
    String.concat " " [s1; "="; s2]
  | If (g, e1, e2) ->
    let s0 = string_of_e g in
    let s1 = string_of_e e1 in
    let s2 = string_of_e e2 in
    String.concat " " ["if"; s0; "{"; s1; "} else {"; s2; "}"]

and string_of_t (t: typ) : string =
  match t with
  | T_any ->
    "any"
  | T_nop ->
    ""
  | T_unit ->
    "unit"
  | T_int ->
    "int"
  | T_bool ->
    "bool"
  | T_str ->
    "str"
  | T_product ts ->
    "(" ^ (String.concat ", " (List.map (fun t -> string_of_t t) ts)) ^ ")"
  | T_sum ts ->
    failwith "unimplemented string_of_t T_sum"
  | T_fun (ts, tr) ->
    (String.concat ", " (List.map (fun t -> string_of_t t) ts)) ^ " >> " ^ (
      string_of_t tr) 
