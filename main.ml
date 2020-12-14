let interpret lexbuf =

  (* Parse expression *)
  Format.printf "Parsing expression...\n";
  Format.print_flush ();
  let e =
    try
      Parser.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at %d:%d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  in
  e |> Pprint.string_of_e |> print_endline;

  (* TODO: Type-check expression *)

  (* Evaluate expression *)
  Format.printf "Evaluating expression...\n";
  Format.print_flush ();
  let v = Eval.eval e in Pprint.string_of_e v

let interpret_str str =
  str |> Lexing.from_string |> interpret

let interpret_file filename =
  filename |> open_in |> Lexing.from_channel |> interpret

let () =
  let filename = Array.get Sys.argv 1 in
  if Sys.file_exists filename
  then filename |> interpret_file |> print_endline
  else failwith ("File " ^ filename ^ " does not exist")