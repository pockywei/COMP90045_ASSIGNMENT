open Bean_ast
open Bean_symbol
open Bean_codegen


let register_count = ref (-1)
let cur_expr_type = ref BeanTypeNone
let cur_param_ref = ref false



let get_cur_LId lvalue = match lvalue with
	| LId(ident) -> ident
  | LField(lvalue,ident) -> ident
  | _ -> (Printf.printf "Error at get_cur_LId"; exit 0)

let get_rest_lvalue lvalue= match lvalue with
  | LField(lvalue,ident) -> lvalue
  | _ -> (Printf.printf "get_rest_lvalue"; exit 0)

(*key should be found in the hashtable other wise it is a type error *)
let rec process_rvalue is_ref lvalue hash_table rvalue = match rvalue with(*{a:int}, {b:int} not the same *)
	| Rexpr(expr) -> let temp_var_stack_num = (getStackNum hash_table (get_cur_LId lvalue)) in
     (cur_func_symbol_hash_table := hash_table ;
		  cur_register_count := 0 ;
		  let _ = codegen_arithmatic expr in (print_store temp_var_stack_num "r0")) (*answer is in r0 *)
  | Rassign (inner_var,inner_rvalue) -> let temp_var_stack_num = (getStackNum hash_table inner_var) in
  	(process_rvalue is_ref lvalue hash_table inner_rvalue;(* a := {b=12+x,c=321}*)
  		if is_ref(*load address to register*)
  		then 
  			(print_load "r1" temp_var_stack_num; (*assign new value to ref var*)
  			print_store_indirect "r1" "r0") (*r1 = r0 *)
  		else print_store 0 "r0")
  | Rstmts (rvalue_list) -> List.iter (process_rvalue is_ref (get_rest_lvalue lvalue) (get_hash_table_symbol (Hashtbl.find hash_table (get_cur_LId lvalue)))) rvalue_list (* a := {a=123,b=321}, if can't find in the hasb table mean error*)
  (* a := {b= ? ,c= ?}*)
  | _ -> (Printf.printf "rvalue processing error \n";exit 0)

let get_symbol_hash_table_primitive_type hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(bean_type , _) -> bean_type
  | S_Int(bean_type , _) -> bean_type
  (*| S_Struct(bean_type , _) -> bean_type*)
  | S_Ref_Int(bean_type , _) -> bean_type
  | S_Ref_Bool(bean_type , _) -> bean_type (*only return Bool or Int, typedef of {} type will cause error*)
  | _ -> (Printf.printf "get primitive type error\n"; exit 0)



let expr_type_match_with_cur_expr_type bean_type = (if ((!cur_expr_type) = BeanTypeNone) then cur_expr_type := bean_type;
	if (bean_type = (!cur_expr_type)) then true else false)

let rec get_lvalue_type hash_table lvalue = match lvalue with
	| LId(ident) -> get_symbol_hash_table_primitive_type hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	(get_lvalue_type temp_hash_symbol_table lvalue_type)
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)




(*for write*)
let rec check_expr_type hash_table expr = match expr with
	| Ebool(bool_val) -> expr_type_match_with_cur_expr_type Bool
  | Eint(int_val) -> expr_type_match_with_cur_expr_type Int
  | Elval(lvalue) -> expr_type_match_with_cur_expr_type (get_lvalue_type hash_table lvalue)
  | Ebinop(expr_1,binop,expr_2) ->(let _ = check_expr_type hash_table expr_1 in check_expr_type hash_table expr_2)
  | Eunop(unop,expr_1) -> check_expr_type hash_table expr_1
  | Eident(ident) -> expr_type_match_with_cur_expr_type (IdentType(ident)) (* "string " *)
  (*| Eident(ident) -> try let temp_type_result = Hashtbl.find hash_table ident in
  	 	expr_type_match_with_cur_expr_type (get_symbol_hash_table_primitive_type(temp_type_result))
  	 with Not_found -> (Printf.printf "checking Eident error when checking expr type\n";exit 0)*)   (*assume ident is in frist level , not with dot*)
  | Ebracket(expr) ->check_expr_type hash_table expr

let rec getStackNum hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  (*| S_Struct(_ , stackNum) -> stackNum*)
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ , stackNum) -> stackNum
  | _ -> (Printf.printf "get stack num error\n"; exit 0)



(*true => ref, false => val *)
let get_bool_ref_val_symbol_hash_table hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , _) -> false
  | S_Int(_ , _) -> false
  | S_Hash(_,_) -> false
  | S_Intext_Hash(_) -> false
  | S_Ref_Hash(_,_) -> true
  | S_Ref_Int(_ , _) -> true
  | S_Ref_Bool(_ , _) -> true (*only return Bool or Int, typedef of {} type will cause error*)
  | S_Ref_Intext_Hash(_) -> true
  | _ -> (Printf.printf "type error for is ref \n";exit 0)

(*true => ref, false => val *)
let get_bool_ref_val_symbol_type symbol_type = match symbol_type with
  | S_Bool(_ , _) -> false
  | S_Int(_ , _) -> false
  | S_Hash(_,_) -> false
  | S_Intext_Hash(_) -> false
  | S_Ref_Hash(_,_) -> true
  | S_Ref_Int(_ , _) -> true
  | S_Ref_Bool(_ , _) -> true (*only return Bool or Int, typedef of {} type will cause error*)
  | S_Ref_Intext_Hash(_) -> true

  (* add intext S_Intext_Hash of (string , symbolTableType) Hashtbl.t , S_Ref_Intext_Hash of (string , symbolTableType) Hashtbl.t *)
  | _ -> (Printf.printf "check val ref type error\n"; exit 0)

let rec get_lvalue_ref_or_not hash_table key_name = match key_name with
	| LId(ident) -> get_bool_ref_val_symbol_hash_table hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_ref_or_not temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)




let rec get_lvalue_symbol_type hash_table lvalue = match lvalue with
	| LId(ident) -> Hashtbl.find hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_symbol_type temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)

(*
 let rec get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum(Hashtbl.find hash_table ident)
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = Hashtbl.find hash_table ident in 
  	get_lvalue_stack_num temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on get stack num lvalue type \n"; exit 0)
*)

let rec get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table =  get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_stack_num temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on get stack num lvalue type \n"; exit 0)


let rec codegen_var_init hash_table one_struct = match one_struct with
	| SingleTypeTermWithIdent(var_name,ListTypeTerm(typedefStruct_list)) -> (try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table var_name) in
			List.iter (codegen_var_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (Printf.printf "codegen_var_init finding symbol table failed at ListTypeTerm\n";exit 0))
	| SingleTypeTermWithIdent(var_name,_) -> print_store (getStackNum hash_table var_name) (get_register_string (!cur_register_count)) 
	| _ -> (Printf.printf "Error on initializing local var codegen_var_init\n";exit 0)

let rec codegen_var_init_incr_ver hash_table one_struct =(incr cur_register_count ;match one_struct with
	| SingleTypeTermWithIdent(var_name,ListTypeTerm(typedefStruct_list)) -> (try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table var_name) in
			List.iter (codegen_var_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (Printf.printf "codegen_var_init finding symbol table failed at ListTypeTerm\n";exit 0))
	| SingleTypeTermWithIdent(var_name,_) -> print_store (getStackNum hash_table var_name) (get_register_string (!cur_register_count)) 
	| _ -> (Printf.printf "Error on initializing local var codegen_var_init\n";exit 0))

let rec codegen_param_init hash_table one_param = match one_param with
  | (Val , ListTypeTerm(typedefStruct_list) , param_name) ->(try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table param_name) in
      List.iter (codegen_var_init_incr_ver temp_hash_symbol_table) typedefStruct_list with
    Not_found -> (Printf.printf "find hash failed codegen_param_init\n";exit 0))
	| (Ref , ListTypeTerm(typedefStruct_list) , param_name) ->(try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table param_name) in
			List.iter (codegen_var_init_incr_ver temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (Printf.printf "find hash failed codegen_param_init\n";exit 0))
	| (_ , _ , param_name) -> print_store (getStackNum hash_table param_name) (get_register_string (!register_count))
	(*| _ -> (Printf.printf "Code Gend Error in codegen_param_init \n";exit 0)*)
(*
let rec tranverse_hash_table_get_data hash_table stack_num = Hashtbl.iter (fun _ value-> match value with
	| S_Ref_Hash of (bean_type,inner_hash_table)
  | S_Hash of (bean_type,inner_hash_table)(*self def type*)
  | S_Bool of (bean_type * stackNum) (*Int => stack num*)
  | S_Int of (bean_type * stackNum)
  | S_Ref_Int of (bean_type * stackNum)
  | S_Ref_Bool of (bean_type * stackNum)(*if beantype is a ident, need to search through typedef hash table*)
  | S_Intext_Hash (inner_hash_table)
  | S_Ref_Intext_Hash of (string , symbolTableType) Hashtbl.t
	| (Printf.printf "Error on tranverse hash table with stack num\n";exit 0)) hash_table
*)


let check_ref_val_type_equal first second = match first with 
  | S_Ref_Hash (bean_type,inner_hash_table) -> (match second with
  	|S_Hash (_,inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	|S_Ref_Hash (_,inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | S_Hash (bean_type,inner_hash_table) -> (match second with
  	|S_Hash (_,inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	|S_Ref_Hash (_,inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | S_Bool (_,_) -> (match second with
  	| S_Bool (_,_) -> true
  	| S_Ref_Bool (_,_) -> true
  	| _ -> false)
  | S_Int (_,_) -> (match second with
  	| S_Int (_,_) -> true
  	| S_Ref_Int (_,_) -> true
  	| _ -> false)
  | S_Ref_Int (_,_) ->  (match second with
  	| S_Int (_,_) -> true
  	| S_Ref_Int (_,_) -> true
  	| _ -> false)
  | S_Ref_Bool (_,_) -> (match second with
  	| S_Bool (_,_) -> true
  	| S_Ref_Bool (_,_) -> true
  	| _ -> false)
  | S_Intext_Hash (inner_hash_table) -> (match second with
  	| S_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| S_Ref_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | S_Ref_Intext_Hash (inner_hash_table) ->( match second with
  	| S_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| S_Ref_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | _ -> (Printf.printf "error check_ref_val_type_equal unwanted type\n";exit 0)

let rec convert_one_expr_param_to_symbol_type one_expr = match one_expr with 
  | Ebool(_) -> S_Bool(Bool,-1) (*-1 means nothing ,just need this struct to compare with clee*)
  | Eint(_) -> S_Int(Int,-1) 
  | Elval(lvalue) -> get_lvalue_symbol_type (!cur_func_symbol_hash_table) lvalue
  | Ebinop (expr_1,binop,expr_2) -> (let _ = convert_one_expr_param_to_symbol_type expr_1 in convert_one_expr_param_to_symbol_type expr_2)
  | Eunop (unop,expr) -> convert_one_expr_param_to_symbol_type expr
  | Ebracket(expr)-> convert_one_expr_param_to_symbol_type expr
  | _ -> (Printf.printf "convert_one_expr_param_to_symbol_type type error\n";exit 0)
  (*| Eident(string_val)*) (*this is for " awefawef  " => type *)

let rec gen_symbol_type_to_register symbol_type is_ref_caller is_ref_callee   =
	match (is_ref_caller,is_ref_callee) with
	| (true,false) -> (match symbol_type with 		
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string !cur_register_count) (get_register_string !cur_register_count) (* dereference r *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string !cur_register_count) (get_register_string !cur_register_count) (* dereference r *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string !cur_register_count) (get_register_string !cur_register_count) (* dereference r *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string !cur_register_count) (get_register_string !cur_register_count) (* dereference r *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (Printf.printf "error on gen_symbol_type_to_register\n";exit 0))
	| (false,true) -> (match symbol_type with 		(* caller not ref, callee ref*)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string !cur_register_count) stack_num; (* r = &s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string !cur_register_count) stack_num; (* r = &s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string !cur_register_count) stack_num; (* r = &s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string !cur_register_count) stack_num; (* r = &s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (Printf.printf "error on gen_symbol_type_to_register\n";exit 0))
	| (true,true) -> (match symbol_type with (* caller ref, callee ref *)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (Printf.printf "error on gen_symbol_type_to_register\n";exit 0))
	| (false,false) -> (match symbol_type with (*caller val, callee val*)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string !cur_register_count) stack_num; (* r = s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (Printf.printf "error on gen_symbol_type_to_register\n";exit 0))


(*one_expr_param is from caller, one_param is from callee*)
let process_calling_method_param caller_hash_table callee_hash_table one_expr_param one_param = match one_param with
	| (Val,_,one_callee_param_name) -> (cur_expr_type := BeanTypeNone;
		(*make sure has same type*)
		if (check_expr_type callee_hash_table one_expr_param)&&(check_ref_val_type_equal (Hashtbl.find callee_hash_table one_callee_param_name) (convert_one_expr_param_to_symbol_type one_expr_param))
		then match one_expr_param with
			| Ebool(bool_val) -> (codegen_arithmatic one_expr_param)
		  | Eint(int_val) -> (codegen_arithmatic one_expr_param)
		  | Elval(lvalue) -> if !cur_expr_type = Int || !cur_expr_type = Bool 
		  	then  codegen_arithmatic one_expr_param
		  	else  (Hashtbl.iter (fun key value -> gen_symbol_type_to_register value (get_lvalue_ref_or_not caller_hash_table lvalue) false ) (get_hash_table_symbol (get_lvalue_symbol_type caller_hash_table lvalue));
		  		!cur_register_count) (*must return a num...*)
		  | Ebinop (expr_1,binop,expr_2) -> (codegen_arithmatic one_expr_param)(* codege_ari incr cur_register inside *)
		  | Eunop (unop,expr) -> (codegen_arithmatic expr)
		  | Ebracket(expr)-> (codegen_arithmatic expr)
		  | _ -> (Printf.printf "process_calling_method_param error => then match one_expr_param with\n";exit 0)
		else (Printf.printf "caller and callee param type mismatch\n"; exit 0))
		(*callee param is val*)
	| (Ref,_,one_callee_param_name) ->  (cur_expr_type := BeanTypeNone;
		(*make sure has same type*)
		if (check_expr_type callee_hash_table one_expr_param) && (check_ref_val_type_equal (Hashtbl.find callee_hash_table one_callee_param_name) (convert_one_expr_param_to_symbol_type one_expr_param))
		then match one_expr_param with
		(* ref does not allow primitive value pass in 
			| Ebool(bool_var) -> (codegen_arithmatic one_expr_param)
		  | Eint(int_val) -> (codegen_arithmatic one_expr_param ) *)
		  | Elval(lvalue) -> if !cur_expr_type = Int || !cur_expr_type = Bool 
		  	then  codegen_arithmatic one_expr_param
		  	else  (Hashtbl.iter (fun key value -> gen_symbol_type_to_register value (get_lvalue_ref_or_not caller_hash_table lvalue) true ) (get_hash_table_symbol (get_lvalue_symbol_type caller_hash_table lvalue));
		  		!cur_register_count) (* passed in is a struct / typedef *)
		  (*| Ebinop (expr_1,binop,expr_2) -> (convert_one_expr_param_to_symbol_type expr_1; convert_one_expr_param_to_symbol_type expr_2) same reason primitive value cant be ref
		  | Eunop (unop,expr) -> convert_one_expr_param_to_symbol_type expr
		  | Ebracket(expr)-> convert_one_expr_param_to_symbol_type expr*)
		  | _ -> (Printf.printf "process_calling_method_param error => then match one_expr_param with\n";exit 0)
		else (Printf.printf "caller and callee param type mismatch\n"; exit 0))(*callee param is  ref *)
	(*| _ -> (Printf.printf "process_calling_method_param error";exit 0)*)

let rec codegen_one_stmt hash_table one_stmt =(cur_func_symbol_hash_table := hash_table; match one_stmt with
	| Assign(lvalue, rvalue) -> (process_rvalue (get_lvalue_ref_or_not hash_table lvalue) lvalue hash_table rvalue) (*set cur_expr_type to lvalue type, then in process_rvalue will check type*)
  | Read(lvalue) ->let temp_lvalue_type = get_lvalue_type hash_table lvalue in 
  	let temp_lvalue_stack_num = (get_lvalue_stack_num hash_table lvalue) in (*ref ?*)
	  	if(temp_lvalue_type = Int)
	  	then 
	  		if (get_lvalue_ref_or_not hash_table lvalue)(* is ref *)
	  		then (print_read_int();
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_int();
	  					print_store temp_lvalue_stack_num "r0")
	  	else 
	  		if (get_lvalue_ref_or_not hash_table lvalue)(* is ref *)
	  		then (print_read_bool(); (*result is in r0 *)
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_bool();
	  					print_store temp_lvalue_stack_num "r0")
  | Write(expr) ->(cur_expr_type := BeanTypeNone; cur_register_count := 0;
    if check_expr_type hash_table expr 
  	then 
  		if(!cur_expr_type = Int) (*write int, result of arithematic is in r0*)
  		then 
  			(let _ =codegen_arithmatic expr in
  				print_print_int())
  		else 
  			  match !cur_expr_type with (*write string*)
  				| IdentType(string_temp) -> (print_string_const (get_register_string 0) string_temp;
  					print_print_string())
          | Bool -> (let _ =codegen_arithmatic expr in
          print_print_bool())
  				| _ -> (Printf.printf "type error when writing a string\n";exit 0)
		else (Printf.printf "write check type failed \n" ; exit 0))
  | Method(method_name, expr_params_list) ->(register_count := -1;
  	print_call method_name;
  	let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find symbol_table_hash method_name) in (*callee hashtable*)
  		let ((callee_func_name,temp_param_list),_,_)  = Hashtbl.find func_param_order_hash_table method_name in(*(valRef*typedefStruct*string) list*) 
		  	(if (List.length expr_params_list) = (List.length temp_param_list)
		  	then (List.iter2 (fun first second -> (let _ = process_calling_method_param hash_table temp_hash_symbol_table first second in Printf.printf "")) expr_params_list temp_param_list )
		  	else (Printf.printf "short for params for calling %s\n" method_name;exit 0)))
  | WhileDec(expr, stmt_list) -> (print_label_by_number !cur_label_count;
  	let while_label = (!cur_label_count) in 
	  	let while_out_label = (!cur_label_count) + 1 in 
		  	(cur_label_count := (!cur_label_count) + 2;
		  		let _ = codegen_arithmatic expr in (*compare condition *)
			  	(print_branch_on_false "r0" (get_label_name while_label); (*branch is different to call .*)
			  	List.iter (codegen_one_stmt hash_table) stmt_list; (**)
			  	print_branch_on_unc (get_label_name while_out_label);
			  	print_label_by_number while_out_label)))
	(*will reserve else label as well, eventhough it might have no else part*)
  | IfDec(expr,then_stmt_list,else_stmt_list) ->(cur_label_count := 0;let if_else_label = (!cur_label_count) in
  	let if_out_label= (!cur_label_count)+1 in ( cur_label_count:=(!cur_label_count)+2;
  		if(List.length (else_stmt_list))!= 0 (*if have else and condition is flase then go to else*)
  		then (cur_register_count := 0;
  			let _ = codegen_arithmatic expr in
  			(print_branch_on_false "r0" (get_label_name if_else_label);
	  			List.iter (codegen_one_stmt hash_table) then_stmt_list ;
	  			print_branch_on_unc (get_label_name if_out_label);
	  			print_label_by_number if_else_label;
	  			List.iter (codegen_one_stmt hash_table) else_stmt_list;(*if false go to else label, if true say in and jump uncondition to out label*)
	  			print_label_by_number if_out_label))
  		else (cur_register_count := 0;
        let _ = codegen_arithmatic expr in 
          (print_branch_on_false "r0" (get_label_name if_out_label);(*if not else and false go to out label*)
  			   List.iter (codegen_one_stmt hash_table) then_stmt_list;
  			   print_label_by_number if_out_label))))(* if not else directly go *) 
  | _ -> (Printf.printf "start_translate_by_function_stmt_list error \n";exit 0))

(*

let check_if_ref func_name key = (*see is var ref or not*)

let check_one_stmt_type stmt_data type_data = (*write, read, assign, *)
*)


let start_translate_by_function_declaration func_name one_functionDeclaration = match one_functionDeclaration with
	|(func_name,funcDecParamList) -> (cur_register_count := -1;
		print_label_by_function_name func_name; (*start each label body*)
		(try let function_symbol_table_hash = get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name) in
			(print_push_stack_frame (Hashtbl.find func_stack_num_hash func_name);
				List.iter (fun x -> (incr cur_register_count;
					codegen_param_init function_symbol_table_hash x)) funcDecParamList;
				cur_register_count := 0)
		with Not_found -> (Printf.printf "%s symbol_table_hash not found\n" func_name; exit 0)))

let rec start_translate_by_function_variable_declaration func_name typedefStruct_list = let var_init_symbol_table = get_hash_table_symbol( Hashtbl.find symbol_table_hash func_name )in
	(cur_register_count := 0;
		print_int_const (get_register_string !cur_register_count) 0; (*initialize everything to 0*)
		List.iter (codegen_var_init var_init_symbol_table)  typedefStruct_list)

let start_translate_by_function_stmt_list func_name stmt_list = let temp_hash_symbol_table = get_hash_table_symbol ( Hashtbl.find symbol_table_hash func_name) in
	( cur_expr_type := BeanTypeNone;
		cur_func_symbol_hash_table := temp_hash_symbol_table;
		List.iter (codegen_one_stmt temp_hash_symbol_table) stmt_list)


let start_translate_by_function one_funcdef = match one_funcdef with
	|((func_name,func_param_list),typedefStruct_list,stmt_list) ->(start_translate_by_function_declaration func_name (func_name,func_param_list);
		start_translate_by_function_variable_declaration func_name typedefStruct_list;
		start_translate_by_function_stmt_list func_name stmt_list;
    print_pop_stack_frame (Hashtbl.find func_stack_num_hash func_name); (*pop stack at the end of function body*)
		print_return ())

(*syntax ?*)
let rec find_funcdef funcdefs func_name = try match (List.hd funcdefs) with
		| ((fun_name,_),_,_) -> if fun_name = func_name then List.hd funcdefs  else find_funcdef (List.tl funcdefs) func_name
	with Failure e-> (Printf.printf "no main function\n";exit 0) 

let start_test_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs);
		(*build_func_method_param_order_hash_table (prog.funcdefs);*)

		Printf.printf "----- Start Printing typdef_table_hash -----\n";
		print_out_one_typedef_table typdef_table_hash;
		Printf.printf "----- End Printing typdef_table_hash -----\n";

		Printf.printf "----- Start Printing symbol_table_hash -----\n";
		print_out_one_symbol_table symbol_table_hash;
		Printf.printf "----- End Printing symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_param_symbol_table_hash -----\n";
		(*print_out_one_symbol_table func_param_symbol_table_hash;*)
		Printf.printf "----- End Printing func_param_symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_stack_num_hash -----\n";
		print_func_stack_num_hash func_stack_num_hash;
		Printf.printf "----- End Printing func_stack_num_hash -----\n";

		Printf.printf "----- Start Generating Oz Code -----\n";
		print_call "main";
		print_halt ();

		let main_def = find_funcdef (prog.funcdefs) ("main") in
			(start_translate_by_function main_def;

		(*List.iter (fun x -> start_translate_by_function x  ) prog.funcdefs;*)
			Printf.printf "----- End Generating Oz Code -----\n"))

let start_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs))
		(*print_out_one_typedef_table typdef_table_hash;*)
		(*print_out_one_symbol_table symbol_table_hash);*)
		(*translate main first *)

