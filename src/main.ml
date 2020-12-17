let syntax_error_msg lexbuf =
  if lexbuf.Lexing.lex_eof_reached
  then Format.printf "Unexpected EOF"
  else 
    let p = lexbuf.Lexing.lex_curr_p in
    let file = p.Lexing.pos_fname in
    let row = p.Lexing.pos_lnum in
    let col = p.Lexing.pos_cnum in

    let rec apply f x n = 
      if n = 0 then x else apply f (f x) (n-1) 
    in

    let span file row col = 
      let f = open_in file in
        (* scan through rows *)
        ignore (apply (fun f -> ignore (input_line f); f) f (row-1));
        let line = input_line f in
        close_in f;
        line
    in


    (* pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol); *)
    Format.printf "Syntax error in %s on line %d:\n%s"
      file (row) (span file row col);
    exit 1

let interpret lexbuf =

  (* Parse expression *)
  Format.printf "Parsing expression...\n";
  Format.print_flush ();
  let e =
    try
      ignore (Parsing.set_trace true);
      Parser.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      syntax_error_msg lexbuf;
      exit 1
  in 
  
  e |> Pprint.string_of_e |> print_endline;

  (* TODO: Type-check expression *)

  (* Evaluate expression *)
  Format.printf "\nEvaluating expression...\n";
  Format.print_flush ();
  let v = Eval.eval e in Pprint.string_of_e v

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
