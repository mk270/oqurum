
let main () =
	let lexbuf = Lexing.from_channel stdin in
		while (Parser.input Lexer.token lexbuf) do
			()
		done

let _ = main ()
