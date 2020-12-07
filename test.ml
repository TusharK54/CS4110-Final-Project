open OUnit2
open Ast
open Main

let tests = [

  "true and true", {|true && true|}, "true";
  "true and false", {|true && false|}, "false";
  "false and true", {|false && true|}, "false";
  "false and false", {|false && false|}, "false";

]

let make_interp_expr_test n in_str out_str =
  n >::
  (fun _ -> assert_equal out_str (interpret_str in_str) ~printer:(fun x-> x))

let suite =
  "suite" >:::
  List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ =
  run_test_tt_main suite
