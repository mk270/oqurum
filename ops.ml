
type location = Location of int

type ident = Ident of string

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

type expressionS =
	| NumS of int
	| VarS of ident
	| AppS of expressionS * expressionS
	| PlusS of expressionS * expressionS
	| BMinusS of expressionS * expressionS
	| MultS of expressionS * expressionS
	| LambdaS of (ident list) * expressionS
	| SetS of ident * expressionS
	| SeqS of expressionS * expressionS
	| IfS of expressionS * expressionS * expressionS

type cell = location * value
type store = cell list

type result = Result of value * store

exception Name_not_found of string
exception Type_error
exception Internal_type_error
exception Not_implemented of string
exception Duplicate_identifier of ident

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

let is_true = function
	| Num n -> n <> 0
	| _ -> true

let rec desugar = function
	| NumS n -> NumC n
	| VarS id -> VarC id
	| AppS (fn, arg) -> AppC (desugar fn, desugar arg)
	| PlusS (l, r) -> PlusC (desugar l, desugar r)
	| MultS (l, r) -> MultC (desugar l, desugar r)
	| BMinusS (l, r) -> PlusC (desugar l, MultC ((NumC (-1)), desugar r))
	| LambdaS (ids, e) -> 
		(* check_unique ids; *)
		let rec fold_lambdac = function
			| [] -> desugar e
			| hd :: tl -> LambdaC (hd, (fold_lambdac tl))
		in
			fold_lambdac ids
		(* LambdaC (List.hd ids, desugar e) *)
	| SetS (id, e) -> SetC (id, desugar e)
	| SeqS (e1, e2) -> SeqC (desugar e1, desugar e2)
	| IfS (e1, e2, e3) -> IfC (desugar e1, desugar e2, desugar e3)

let rec interp expr env sto =
	match expr with
		| NumC n -> Result (Num n, sto)
		| VarC n -> 
			Result ((fetch (lookup n env) sto), sto)
		| LambdaC (a, b) -> Result (Closure (a, b, env), sto)
		| SeqC (b1, b2) -> 
(*			logf "in SeqC\n";
			dump_store sto; *)
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
		| IfC (t, e1, e2) -> 
			(match (interp t env sto) with
				| Result (value_t, store_t) ->
					let e = 
						if is_true value_t
						then e1
						else e2
					in
						interp e env store_t)

let rec dump_store = function 
	| [] -> logf "[endofstore]\n"
	| hd :: tl -> let l, v = hd in 
					  logf "loc: %s\n" (string_of_location l); 
					  logf "val: %s\n" (string_of_value v);
					  dump_store tl		

let eval_and_print v =
	let result = interp (desugar v) empty_env empty_storage in
		match result with
			| Result (vl, store) -> let s = string_of_value vl in
										print_endline s;
										flush stdout;
										s
