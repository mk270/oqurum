
(*
  Oqurum, a toy language

  Copyright (C) 2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero Public License v3.0
*)

type ident = Ident of string

type location = Location of int

type binding = ident * location
type environment = binding list

type value =
	| Num of int
	| Closure of ident * expressionC * environment
and 
	expressionC =
	| NumC of int
	| VarC of ident
	| AppC of expressionC * expressionC
	| PlusC of expressionC * expressionC
	| MultC of expressionC * expressionC
	| LambdaC of ident * expressionC
	| SetC of ident * expressionC
	| SeqC of expressionC * expressionC
	| IfC of expressionC * expressionC * expressionC
	| LessThanEqC of expressionC * expressionC

type expressionS =
	| NumS of int
	| VarS of ident
	| AppS of expressionS * (expressionS list)
	| PlusS of expressionS * expressionS
	| BMinusS of expressionS * expressionS
	| MultS of expressionS * expressionS
	| LambdaS of (ident list) * expressionS
	| SetS of ident * expressionS
	| SeqS of expressionS * expressionS
	| IfS of expressionS * expressionS * expressionS
	| DefVarS of ident * expressionS * expressionS
	| DefFuncS of ident * (ident list) * expressionS * expressionS
	| LessThanEqS of expressionS * expressionS

let string_of_location = function
	| Location l -> "@" ^ (string_of_int l)

let string_of_ident = function
	| Ident s -> "$" ^ s


let string_of_expression exp =
	let rec string_of_expression = function
		| NumC n -> string_of_int n ^ " : int"	
		| VarC id -> string_of_ident id
		| LambdaC (id, e1) -> string_of_id_e1 id e1 "lambda"
		| SetC (id, e1) -> string_of_id_e1 id e1 "set!"
		| AppC (e1, e2) -> string_of_e1_e2 e1 e2 "apply"
		| PlusC (e1, e2) -> string_of_e1_e2 e1 e2 "+" 
		| MultC (e1, e2) -> string_of_e1_e2 e1 e2 "*"
		| SeqC (e1, e2) -> string_of_e1_e2 e1 e2 "seq"
		| LessThanEqC (e1, e2) -> string_of_e1_e2 e1 e2 "lte"
		| IfC (e1, e2, e3) -> "if(" ^ string_of_expression e1 ^
			", " ^ string_of_expression e2 ^
			", " ^ string_of_expression e3 ^ ")"
	and string_of_e1_e2 e1 e2 name =
		name ^ "(" ^ string_of_expression e1 ^ ", " ^
			string_of_expression e2 ^ ")"
	and string_of_id_e1 id e1 name =
		name ^ "(" ^ string_of_ident id ^ ") { " ^
			string_of_expression e1 ^ " }"
	in
		string_of_expression exp

let string_of_value = function
	| Num n -> string_of_int n
	| Closure (id, e1, _) -> "closure(" ^ (string_of_ident id) ^ ") {" ^
		string_of_expression e1 ^ "}"
