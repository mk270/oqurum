
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
