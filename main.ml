
(*
  Oqurum, a toy language

  Copyright (C) 2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero Public License v3.0
*)

let main () =
	let lexbuf = Lexing.from_channel stdin in
		while (Parser.input Lexer.token lexbuf) do
			()
		done

let _ = main ()
