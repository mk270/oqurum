
type ident = Ident of string

type value =
	| Num of int
	| Closure of ident * expressionC * environment
and
	binding = ident * value
and
	environment = binding list
and 
	expressionC =
	| NumC of int
	| IdC of ident
	| AppC of expressionC * expressionC
	| PlusC of expressionC * expressionC
	| MultC of expressionC * expressionC
	| LambdaC of ident * expressionC

exception Name_not_found
exception Type_error
exception Internal_type_error

let empty_env : environment = []
let extend_env new_binding env = new_binding :: env
			
let rec lookup look_for env =
	match env with
		| [] -> raise Name_not_found
		| hd :: tl ->
			if look_for = fst hd
			then snd hd
			else lookup look_for tl

let num_plus l r =
	match l, r with
		| Num l', Num r' -> Num (l' + r')
		| _ -> raise Type_error

let num_mult l r =
	match l, r with
		| Num l', Num r' -> Num (l' * r')
		| _ -> raise Type_error

let rec interp expr env =
	match expr with
		| NumC n -> Num n
		| IdC n -> lookup n env
		| PlusC (l, r) -> num_plus (interp l env) (interp r env)
		| MultC (l, r) -> num_mult (interp l env) (interp r env)
		| LambdaC (a, b) -> Closure (a, b, env)
		| AppC (f, a) -> 
			let f_value = interp f env in
				match f_value with
					| Num _ -> raise Internal_type_error
					| Closure (clos_arg, clos_body, clos_env) ->
						let new_binding = clos_arg, interp a env in
						let extended_env = extend_env new_binding clos_env
						in
							interp clos_body extended_env

let string_of_ident = function
	| Ident s -> "$" ^ s

let string_of_value = function
	| Num n -> string_of_int n
	| Closure (id, _, _) -> "lambda(" ^ ") {...}"

let eval_and_print v =
	let vl = interp v empty_env in
	let s = string_of_value vl in
		print_endline s;
		flush stdout;
		s
