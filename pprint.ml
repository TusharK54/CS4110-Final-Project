open Ast

let rec string_of_e e =
  match e with
  | Unit ->
  "()"
  | Int n ->
  let s = string_of_int n in
  if n < 0 then String.concat "" ["("; s; ")"]
  else s
  | Bool b ->
  if b then "true" else "false"
  | Str s ->
  s
  | Var (x, None) ->
  x
  | Var (x, Some t) ->
  let s = string_of_t t in
  String.concat "" ["("; x; " : "; s; ")"]
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
      | Assert ->
      String.concat "" ["assert "; s]
      | Not ->
      String.concat "" ["(!"; s; ")"]
    end
  | Seq (e1, e2) ->
    let s1 = string_of_e e1 in
    let s2 = string_of_e e2 in
    String.concat "" [s1; ";\n"; s2]
  | Assign (x, e) ->
    let s = string_of_e e in
    String.concat " " [x; "="; s]
  | If (g, e1, e2) ->
    let s0 = string_of_e g in
    let s1 = string_of_e e1 in
    let s2 = string_of_e e2 in
    String.concat " " ["if"; s0; "{"; s1; "} else {"; s2; "}"]

and string_of_t t =
  match t with
  | T_int ->
  "int"
  | T_bool ->
  "bool"
  | T_str ->
  "str"
  | T_unit ->
  "unit"
  | T_fun (t_args, t_out) ->
  ""
  | T_sum t_list ->
  ""
  | T_product t_list ->
  ""
