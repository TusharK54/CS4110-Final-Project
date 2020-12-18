open Eval
open Check
open Errors

let print_exp exp =
  exp |> Pprint.string_of_e |> print_endline
let print_typ typ =
  typ |> Pprint.string_of_t |> print_endline
let print_info info =
  ANSITerminal.(print_string [blue] (String.uppercase_ascii info));
  print_string " -- "
let print_ok () = 
  ANSITerminal.(print_string [green] "OK\n");
  flush stdout
let print_error () = 
  ANSITerminal.(print_string [red] "ERROR\n");
  flush stdout

let print_file_error filename =
  print_error ();
  Format.printf "File \"%s\" not found" filename;
  Format.print_flush ()

let print_eof_error () =
  print_error ();
  Format.printf "unexpected end-of-file";
  Format.print_flush ()

let handle_error e =
  print_error ();
  match e with
  | Errors.EvalError (msg, exp) ->
    if String.length msg > 0 then print_endline (msg^":");
    if String.length exp > 0 then print_endline exp;
    exit 1
  | Errors.TypeError (msg, exp) ->
    if String.length msg > 0 then print_endline (msg^":");
    if String.length exp > 0 then print_endline exp;
    exit 1
  | Errors.RuntimeError (msg, exp) ->
    if String.length msg > 0 then print_endline (msg^":");
    if String.length exp > 0 then print_endline exp;
    exit 2
  | Errors.UncaughtTypeError (msg, exp) ->
    print_endline ("Uncaught type error: "^msg);
    if String.length exp > 0 then print_endline exp;
    exit 2
  | e -> 
    raise e 

let print_syntax_error lexbuf =
  print_error ();
  (* lexing position info *)
  let p_s = lexbuf.Lexing.lex_start_p in
  let p_e = lexbuf.Lexing.lex_curr_p in
  (* error span position info *)
  let file = p_s.Lexing.pos_fname in
  let row = p_s.Lexing.pos_lnum in
  let span_s = (p_s.Lexing.pos_cnum - p_s.Lexing.pos_bol) in
  let span_e = (p_e.Lexing.pos_cnum - p_e.Lexing.pos_bol) in

  (* helper function *)
  let rec apply f x n = 
    if n = 0 then x else apply f (f x) (n-1) 
  in

  (* actual line in the file where the error occured *)
  let line = 
    let f = open_in file in
      (* scan past irrelevant rows  *)
      ignore (apply (fun f -> ignore (input_line f); f) f (row-1));
      (* read line where error occured *)
      let l = input_line f in
      close_in f; l
  in

  (* segment line according to where error is *)
  let pre_error = String.sub line 0 span_s in
  let error = String.sub line span_s (span_e - span_s) in
  let post_error = String.sub line span_e (String.length line - span_e) in

  (* print color-formatted error message *)
  Format.printf "Syntax error in file \"%s\" on line %d:\n" file row;
  Format.print_flush ();
  print_string pre_error;
  ANSITerminal.(print_string [on_red] error);
  print_string post_error

let interpret lexbuf (verbose: bool) =
  (* parse expression *)
  if verbose then print_info "Parsing";
  let e =
    try
      Parser.program Lexer.token lexbuf
    with 
    | Parsing.Parse_error ->
      print_syntax_error lexbuf;
      exit 1
    | End_of_file ->
      print_eof_error ();
      exit 1
  in
  if verbose then (
    print_ok ();
    print_exp e
  );

  (* type-check expression *)
  if verbose then print_info "Type-Checking";
  let t = 
    try
      check e
    with
      | err ->
        handle_error err
  in
  if verbose then (
    print_ok ();
    print_typ t
  );

  (* evaluate expression *)
  if verbose then print_info "Executing";
  let v = 
    try
      eval e
    with 
    | err ->
      handle_error err
  in
  if verbose then (
    print_ok ();
    print_exp v
  );

  (* return pretty-printed result *)
  v

let interpret_str str =
  let lexbuf = Lexing.from_string str in
  interpret lexbuf false

let interpret_file filename verbose =
  try
    let lexbuf = filename |> open_in |> Lexing.from_channel in
    Lexing.set_filename lexbuf filename;
    interpret lexbuf verbose
  with
    | Sys_error err -> 
      print_file_error filename;
      exit 1

(* CLI entry-point *)
let () =
  (* get args from command line *)
  let filename = Array.get Sys.argv 1 in
  let verbose = true in
  let e = interpret_file filename verbose in
  if not verbose then print_exp e
    
