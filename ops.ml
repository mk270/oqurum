
type location = Location of int

type ident = Ident of string

type value =
	| Num of int
	| Closure of ident * expressionC * environment
and
	binding = ident * location
and
	environment = binding list
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

type cell = location * value
type store = cell list

type result = Result of value * store

exception Name_not_found of string
exception Type_error
exception Internal_type_error
exception Not_implemented of string

let empty_env : environment = []
let extend_env new_binding env = new_binding :: env

let empty_storage : store = []
let override_store new_cell sto = new_cell :: sto

let logf = Printf.printf

let string_of_location = function
	| Location l -> "@" ^ (string_of_int l)

let string_of_value = function
	| Num n -> string_of_int n
	| Closure (id, _, _) -> "lambda(" ^ ") {...}"

let rec dump_store = function 
	| [] -> logf "[endofstore]\n"
	| hd :: tl -> let l, v = hd in 
					  logf "loc: %s\n" (string_of_location l); 
					  logf "val: %s\n" (string_of_value v);
					  dump_store tl

let counter = ref 0

let string_of_ident = function
	| Ident s -> "$" ^ s

let new_loc () =
	(counter := !counter + 1;
	 Location !counter)

let rec lookup look_for env =
	match env with
		| [] -> raise (Name_not_found (string_of_ident look_for))
		| hd :: tl ->
			if look_for = fst hd
			then snd hd
			else lookup look_for tl

let rec fetch look_for sto =
	match sto with
		| [] -> raise Not_found
		| hd :: tl ->
			if look_for = fst hd
			then snd hd
			else fetch look_for tl

let num_plus l r =
	match l, r with
		| Num l', Num r' -> Num (l' + r')
		| _ -> raise Type_error

let num_mult l r =
	match l, r with
		| Num l', Num r' -> Num (l' * r')
		| _ -> raise Type_error

let rec interp expr env sto =
	match expr with
		| NumC n -> Result (Num n, sto)
		| VarC n -> 
			Result ((fetch (lookup n env) sto), sto)
		| LambdaC (a, b) -> Result (Closure (a, b, env), sto)
		| SeqC (b1, b2) -> 
			logf "in SeqC\n";
			dump_store sto;
			(match (interp b1 env sto) with
				| Result (value_b1, store_b1) ->
					interp b2 env store_b1)
		| PlusC (l, r) -> 
			(match (interp l env sto) with
				| Result (value_l, store_l) ->
					(match (interp r env store_l) with
						| Result (value_r, store_r) ->
							Result ((num_plus value_l value_r), store_r)))
		| MultC (l, r) -> 
			(match (interp l env sto) with
				| Result (value_l, store_l) ->
					(match (interp r env store_l) with
						| Result (value_r, store_r) ->
							Result ((num_mult value_l value_r), store_r)))
		| AppC (f, a) 
			-> (match (interp f env sto) with
				| Result (value_f, store_f) ->
					(match (interp a env store_f) with
						| Result (value_a, store_a) ->
							let where = new_loc () in
								(match value_f with
									| Closure (value_f_arg, value_f_body, value_f_env) ->
										interp 
											value_f_body 
											(extend_env (value_f_arg, where) value_f_env)
											(override_store (where, value_a) store_a)
									| _ -> raise Internal_type_error)))
		| SetC (var, val_) 
			-> (match (interp val_ env sto) with
				| Result (value_val, store_val) ->
					let where = lookup var env in
						Result (value_val, 
								override_store (where, value_val) store_val))

let eval_and_print v =
	let result = interp v empty_env empty_storage in
		match result with
			| Result (vl, store) -> let s = string_of_value vl in
										print_endline s;
										flush stdout;
										s
