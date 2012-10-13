
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
	| IDENTIFIER    { IdC (Ident $1) }
	| LBRACE braced_exp RBRACE { $2 }
;
braced_exp:
	| PLUS      exp exp { PlusC  ($2, $3) }
	| MULTIPLY  exp exp { MultC  ($2, $3) }
	| APPLY     exp exp { AppC ($2, $3) }
	| LAMBDA LPAREN ident RPAREN exp { LambdaC ($3, $5) }
;
ident:
	| IDENTIFIER { Ident $1 }
;

