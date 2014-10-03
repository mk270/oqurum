
(*
  Oqurum, a toy language

  Copyright (C) 2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero Public License v3.0
*)

let main () =
	let lexbuf = Lexing.from_channel stdin in

	let report_lex () =
		let pos = lexbuf.Lexing.lex_curr_p in
		let lnum = pos.Lexing.pos_lnum in
		let cnum = pos.Lexing.pos_cnum in
		let lexeme = Lexing.lexeme lexbuf in 
		Printf.printf "line: %d, byte: %d, [%s]\n" 
			lnum cnum lexeme
	in

	let parse () = 
		try let rv = Parser.input Lexer.token lexbuf in
				rv
		with 
		| Failure f ->
			report_lex ();
			print_endline f;
			failwith f
		| Parsing.Parse_error ->
			report_lex ();
			failwith "parse error"
	in
		
		while (parse ()) do
			()
		done

let _ = main ()
