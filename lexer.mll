{
  open Parser
  open Lexing
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}
let digit = ['0'-'9']
let letter = ['a'-'z']
let nonquote = [ ^ '"' ]
let nonnl = [ ^ '\n' ]

rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'		{ incr_lineno lexbuf; token lexbuf }
  | "-"? digit+ as num { NUM (int_of_string num) }
  | "apply" { APPLY }
  | "assign" { ASSIGN }
  | "lambda" { LAMBDA }
  | "seq"    { SEQ }
  | "if"     { IF }
  | letter+ as id { IDENTIFIER id }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '+'		{ PLUS }
  | '-'     { MINUS }
  | '*'     { MULTIPLY }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
(*  | _		{ token lexbuf } *)
  | '#' nonnl*  { token lexbuf }
  | eof		{ EOF }
