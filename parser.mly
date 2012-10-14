
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
	| NUM			{ NumC $1 }
	| IDENTIFIER    { VarC (Ident $1) }
	| LAMBDA LPAREN ident RPAREN braced_exp { LambdaC ($3, $5) }
	| braced_exp    { $1 }
;
braced_exp:
	| LBRACE inner_exp RBRACE { $2 }
;
inner_exp:
	| PLUS exp exp     { PlusC  ($2, $3) }
	| MULTIPLY exp exp { MultC  ($2, $3) }
	| APPLY exp exp    { AppC   ($2, $3) }
	| SEQ exp exp      { SeqC   ($2, $3) }
	| ASSIGN ident exp { SetC   ($2, $3) }
;
ident:
	| IDENTIFIER { Ident $1 }
;

