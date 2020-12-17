let eof_error_msg () =
  ANSITerminal.(print_string [red] "Unexpected EOF\n")

let syntax_error_msg lexbuf =
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
  ANSITerminal.(print_string [red] "ERROR\n");
  Format.printf "File %s on line %d:\n" file row;
  Format.print_flush ();
  print_string pre_error;
  ANSITerminal.(print_string [on_red] error);
  print_string post_error

let eval_error_msg error =
  ANSITerminal.(print_string [red] "ERROR\n")

let interpret lexbuf =
  (* parse expression *)
  Format.printf "\nSytnactic analysis ... ";
  Format.print_flush ();
  let e =
    try
      Parser.program Lexer.token lexbuf
    with 
    | Parsing.Parse_error ->
      syntax_error_msg lexbuf;
      exit 1
    | End_of_file ->
      eof_error_msg ();
      exit 1
  in 
  (* pretty-print expression *)
  ANSITerminal.(print_string [green] "GOOD\n");
  e |> Pprint.string_of_e |> print_endline;

  (* type-check expression TODO *)
  Format.printf "\nType-checking ... \n";
  Format.print_flush ();

  (* evaluate expression *)
  Format.printf "\nRunning program ... ";
  Format.print_flush ();
  let v = 
    try
      Eval.eval e
    with Eval.RuntimeError err ->
      (* TODO: better handling *)
      eval_error_msg ();
      print_string err;
      exit 2
  in 
  (* pretty-print result *)
  ANSITerminal.(print_string [green] "GOOD\n");
  (* v |> Pprint.string_of_e |> print_endline; *)

  (* return pretty-printed result *)
  v |> Pprint.string_of_e

let interpret_str str =
  str |> Lexing.from_string |> interpret

let interpret_file filename =
  let lexbuf = filename |> open_in |> Lexing.from_channel in
  Lexing.set_filename lexbuf filename;
  interpret lexbuf

let () =
  let filename = Array.get Sys.argv 1 in
  if Sys.file_exists filename
  then filename |> interpret_file |> print_endline
  else failwith ("File " ^ filename ^ " does not exist")
