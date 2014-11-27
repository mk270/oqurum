
%{

(*
  Oqurum, a toy language

  Copyright (C) 2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero Public License v3.0
*)

open Printf
open Lexing
open Ast

let parse_error s = Printf.printf "Parse error: %s\n" s


%}

/* Ocamlyacc Declarations */
%token NEWLINE
%token LPAREN RPAREN
%token <int> NUM
%token <string> STRING
%token PLUS
%token MINUS
%token MULTIPLY
%token SEMICOLON
%token LBRACE RBRACE
%token <string> IDENTIFIER
%token EOF
%token WITH
%token FUNDEF
%token DEFVAR
%token DEFFUNC
%token APPLY
%token LAMBDA
%token ASSIGN
%token SEQ
%token IF
%token COMMA
%token LTE

%left PLUS MINUS
%left MULTIPLY
%left COMMA
%right CARET	/* exponentiation */

%start input
%type <bool> input

/* Grammar follows */
%%
input:
    | EOF { false } 
	| cunit	input { true }
;
cunit:	
	| exp    { register $1 }
;
exp:
	| NUM			{ NumS $1 }
	| IDENTIFIER    { VarS (Ident $1) }
	| braced_exp    { $1 }
;
braced_exp:
	| LBRACE inner_exp RBRACE { $2 }
;
inner_exp:
	| LAMBDA LPAREN ident_list_wrap RPAREN exp { LambdaS ($3, $5) }
	| SEQ exp exp      { SeqS   ($2, $3) }
	| ASSIGN ident exp { SetS   ($2, $3) }
	| IF exp exp exp   { IfS    ($2, $3, $4) }
	| APPLY exp exp_list { AppS   ($2, $3) }
	| PLUS exp exp     { PlusS  ($2, $3) }
	| MINUS exp exp    { BMinusS ($2, $3) } 
	| MULTIPLY exp exp { MultS  ($2, $3) }
	| DEFVAR LPAREN ident exp RPAREN exp { DefVarS ($3, $4, $6) }
	| DEFFUNC ident LPAREN ident_list RPAREN exp exp 
			{ DefFuncS ($2, $4, $6, $7) }
	| LTE exp exp { LessThanEqS ($2, $3) }
;
ident_list_wrap:
	| ident_list { List.rev $1 }
;
ident_list:
	| ident_list COMMA ident { $3 :: $1 }
	| ident { [ $1 ] }
;
ident:
	| IDENTIFIER { Ident $1 }
;
exp_list:
	| exp_list exp { $2 :: $1 }
	| exp { [ $1 ] }
;
