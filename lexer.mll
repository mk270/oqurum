
(*
  Oqurum, a toy language

  Copyright (C) 2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero Public License v3.0
*)


{
  open Parser
  open Lexing

  exception Unexpected_token

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  let keyword_table = Hashtbl.create 72
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
               Keywords.keywords

}
let digit = ['0'-'9']
let letter = ['a'-'z']
let alnum = (letter | digit)
let identifier = letter alnum*
let nonquote = [ ^ '"' ]
let nonnl = [ ^ '\n' ]

rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'		{ incr_lineno lexbuf; token lexbuf }
  | "-"? digit+ as num { NUM (int_of_string num) }
  | identifier as id 
		  { try Hashtbl.find keyword_table id
			with Not_found -> IDENTIFIER id }
  | "<="    { LTE }
  | ','     { COMMA }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '+'		{ PLUS }
  | '-'     { MINUS }
  | '*'     { MULTIPLY }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
(*  | _		{ token lexbuf } *)
  | '#' nonnl*  { token lexbuf }
  | _ { raise Unexpected_token }
  | eof		{ EOF }


(*  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
*)
