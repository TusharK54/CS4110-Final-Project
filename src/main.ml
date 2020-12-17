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
  Format.printf "File %s on line %d:\n" file row;
  Format.print_flush ();
  print_string pre_error;
  ANSITerminal.(print_string [on_red] error);
  print_string post_error
  
let eval_error_msg error =
  Format.printf "%s" error;
  Format.print_flush ()
let file_error_msg filename =
  Format.printf "%s not found" filename;
  Format.print_flush ()

let eof_error_msg () =
  Format.printf "Unexpected EOF";
  Format.print_flush ()

let interpret lexbuf (verbose: bool) =
  let error_msg () = 
    if verbose then (
      ANSITerminal.(print_string [red] "ERROR\n");
      flush stdout
    )
  in
  let good_msg () = 
    if verbose then (
      ANSITerminal.(print_string [green] "DONE\n");
      flush stdout
    )
  in
  let info_msg msg =
    if verbose then (
      ANSITerminal.(print_string [blue] (String.uppercase_ascii msg));
      print_string " >> ";
    )
  in

  (* parse expression *)
  info_msg "Parsing";  
  let e =
    try
      Parser.program Lexer.token lexbuf
    with 
    | Parsing.Parse_error ->
      error_msg ();
      syntax_error_msg lexbuf;
      exit 1
    | End_of_file ->
      error_msg ();
      eof_error_msg ();
      exit 1
  in 
  good_msg ();

  (* type-check expression TODO *)
  info_msg "Type-Checking";
  good_msg ();

  (* evaluate expression *)
  info_msg "Executing";
  let v = 
    try
      Eval.eval e
    with 
    | Eval.RuntimeError err ->
      (* TODO: better handling *)
      error_msg ();
      eval_error_msg err;
      exit 2
  in
  good_msg ();

  (* return pretty-printed result *)
  v

let interpret_str str =
  let lexbuf = Lexing.from_string str in
  interpret lexbuf false

let interpret_file filename verbose =
  let lexbuf = filename |> open_in |> Lexing.from_channel in
  Lexing.set_filename lexbuf filename;
  interpret lexbuf verbose

(* CLI entry-point *)
let () =
  (* get args from command line *)
  let filename = Array.get Sys.argv 1 in
  let verbose = true in
  if not (Sys.file_exists filename) then  
    file_error_msg filename
  else
    interpret_file filename verbose
    |> Pprint.string_of_e |> print_endline 
