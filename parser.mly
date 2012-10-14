
%{
open Printf
open Lexing
open Ops
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
%token APPLY
%token LAMBDA
%token ASSIGN
%token SEQ
%token IF

%left PLUS MINUS
%left MULTIPLY
%right CARET	/* exponentiation */

%start input
%type <bool> input

/* Grammar follows */
%%
input:
    | EOF { true } 
	| cunit	input { false }
;
cunit:	
	| exp    { Ops.eval_and_print $1 }
;
exp:
	| NUM			{ NumS $1 }
	| IDENTIFIER    { VarS (Ident $1) }
	| LAMBDA LPAREN ident RPAREN braced_exp { LambdaS ($3, $5) }
	| braced_exp    { $1 }
;
braced_exp:
	| LBRACE inner_exp RBRACE { $2 }
;
inner_exp:
	| PLUS exp exp     { PlusS  ($2, $3) }
	| MINUS exp exp    { BMinusS ($2, $3) } 
	| MULTIPLY exp exp { MultS  ($2, $3) }
	| APPLY exp exp    { AppS   ($2, $3) }
	| SEQ exp exp      { SeqS   ($2, $3) }
	| ASSIGN ident exp { SetS   ($2, $3) }
	| IF exp exp exp   { IfS    ($2, $3, $4) }
;
ident:
	| IDENTIFIER { Ident $1 }
;

