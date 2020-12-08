open Ast

let rec to_string e =
  match e with
  | Unit ->
  "()"
  | Int n ->
  string_of_int n
  | Bool b ->
  if b then "true" else "false"
  | Str s ->
  s
  | Var x ->
  x
  | Bop (op, e1, e2) ->
    begin
      let s1 = to_string e1 in
      let s2 = to_string e2 in
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
      let s = to_string e in
      match op with
      | Negate ->
      String.concat "" ["(-"; s; ")"]
      | Not ->
      String.concat "" ["(!"; s; ")"]
    end
  | Seq (e1, e2) ->
    let s1 = to_string e1 in
    let s2 = to_string e2 in
    String.concat "" [s1; ";\n"; s2]
  | Assign (x, e) ->
    let s = to_string e in
    String.concat " " [x; "="; s]
  | If (g, e1, e2) ->
    let s0 = to_string g in
    let s1 = to_string e1 in
    let s2 = to_string e2 in
    String.concat " " ["if"; s0; "{"; s1; "} else {"; s2; "}"]
