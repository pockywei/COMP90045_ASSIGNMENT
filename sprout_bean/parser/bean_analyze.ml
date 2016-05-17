open Bean_ast
open Bean_symbol
open Bean_codegen

let register_count = ref (-1)
let cur_expr_type = ref BeanTypeNone


let get_cur_LId lvalue =
	| LId(ident) -> ident
  | LField(lvalue,ident) -> ident
  | _ -> (Printf.printf "Error at get_cur_LId"; exit 0)

let get_rest_lvalue =
  | LField(lvalue,ident) -> lvalue
  | _ -> (Printf.printf "get_rest_lvalue"; exit 0)

(*key should be found in the hashtable other wise it is a type error *)
let rec process_rvalue is_ref lvalue hash_table rvalue = match rvalue (*{a:int}, {b:int} not the same *)
	| Rexpr(expr) -> (cur_func_symbol_hash_table = hash_table ;
		cur_register_count := 0 ;
		codegen_arithmatic expr) (*answer is in r0 *)
  | Rassign (inner_var,inner_rvalue) try let temp_var_stack_num = getStackNum( Hashtbl.find hash_table inner_var )in
  	(process_rvalue temp_hash_symbol_table inner_rvalue;
  		if is_ref(*load address to register*)
  		then 
  			print_load "r1" temp_var_stack_num (*assign new value to ref var*)
  			print_store_indirect "r1" "r0" (*r1 = *r0 *)
  		else print_store 0 "r0"
		with Not_found -> (Printf.printf "rvalue type error\n";exit 0)
  | Rstmts (rvalue_list) -> List.iter (process_rvalue (get_rest_lvalue lvalue) ( get_hash_table_symbol (Hashtbl.find hash_table (get_cur_LId lvalue))) rvalue_list (* a := {a=123,b=321}, if can't find in the hasb table mean error*)
  | _ -> (Printf.printf "rvalue processing error \n",)

let expr_type_match_with_cur_expr_type bean_type = (if ((!cur_expr_type) = BeanTypeNone) then cur_expr_type := bean_type;
	if (bean_type = (!cur_expr_type)) then true else false)

let check_expr_type hash_table expr = match expr with
	| Ebool(bool_val) -> expr_type_match_with_cur_expr_type Bool
  | Eint(int_val) -> expr_type_match_with_cur_expr_type Int
  | Elval(lvalue) -> expr_type_match_with_cur_expr_type (get_lvalue_type lvalue)
  | Ebinop(expr,binop,expr) ->(get_expr_type hash_table expr;get_expr_type hash_table expr)
  | Eunop(unop,expr) -> get_expr_type expr
  | Eident(ident) -> try let temp_type_result = Hashtbl.find hash_table ident in
  	 	expr_type_match_with_cur_expr_type (get_symbol_hash_table_primitive_type(temp_type_result))
  	 with Not_found -> (Printf.printf "checking Eident error when checking expr type\n";exit 0)   (*assume ident is in frist level , not with dot*)
  | Ebracket(expr) ->get_expr_type hash_table expr

let rec getStackNum hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  | S_Struct(_ , stackNum) -> stackNum
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ , stackNum) -> stackNum
  | _ -> (Printf.printf "get stack num error\n"; exit 0)

let get_symbol_hash_table_primitive_type hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(bean_type , _) -> bean_type
  | S_Int(bean_type , _) -> bean_type
  | S_Struct(bean_type , _) -> bean_type
  | S_Ref_Int(bean_type , _) -> bean_type
  | S_Ref_Bool(bean_type , _) -> bean_type (*only return Bool or Int, typedef of {} type will cause error*)
  | _ -> (Printf.printf "get primitive type error\n"; exit 0)

(*true => ref, false => val *)
let get_bool_ref_val_symbol_hash_table hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , _) -> false
  | S_Int(_ , _) -> false
  | S_Hash(_,_) -> false
  | S_Ref_Hash(_,_) -> true
  | S_Ref_Int(_ , _) -> true
  | S_Ref_Bool(_ , _) -> true (*only return Bool or Int, typedef of {} type will cause error*)
  | _ -> (Printf.printf "check val ref type error\n"; exit 0)

let get_lvalue_ref_or_not hash_table key_name =
	| LId(ident) -> get_bool_ref_val_symbol_hash_table(Hashtbl.find hash_table ident)
  | LField of (lvalue_type,ident) -> let temp_hash_symbol_table = Hashtbl.find hash_table ident in 
  	get_lvalue_type temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)

let get_lvalue_type hash_table lvalue = match lvalue with
	| LId(ident) -> get_symbol_hash_table_primitive_type(Hashtbl.find hash_table ident)
  | LField of (lvalue_type,ident) -> let temp_hash_symbol_table = Hashtbl.find hash_table ident in 
  	get_lvalue_type temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)

 let get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum(Hashtbl.find hash_table ident)
  | LField of (lvalue_type,ident) -> let temp_hash_symbol_table = Hashtbl.find hash_table ident in 
  	get_lvalue_type temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)

let rec codegen_var_init hash_table one_struct = match one_struct with
	| SingleTypeTermWithIdent(var_name,ListTypeTerm(typedefStruct_list)) -> try let temp_hash_symbol_table = (Hashtbl.find hash_table var_name) in
			List.iter (codegen_var_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (Printf.printf "codegen_var_init finding symbol table failed at ListTypeTerm\n";exit 0)
	| SingleTypeTermWithIdent(var_name,_) -> print_store (getStackNum hash_table param_name) (get_register_string register_count) 
	| _ -> (Printf.printf "Error on initializing local var codegen_var_init\n";exit 0);

let rec codegen_param_init hash_table one_param = match one_param with
	| (Ref , ListTypeTerm(typedefStruct_list) , param_name) ->try let temp_hash_symbol_table = (Hashtbl.find hash_table param_name) in
			List.iter (codegen_param_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (Printf.printf "find hash failed codegen_param_init\n";exit 0)
	| (_ , _ , param_name) -> print_store (getStackNum hash_table param_name) (get_register_string register_count) 
	| _ -> (Printf.printf "Code Gend Error in codegen_param_init \n";exit 0)

let process_calling_method_param hash_table one_expr = 

let rec codegen_one_stmt hash_table one_stmt = match one_stmt with
	| Assign(lvalue, rvalue) -> (;
		process_rvalue (get_lvalue_ref_or_not lvalue hash_table lvalue) lvalue rvalue) (*set cur_expr_type to lvalue type, then in process_rvalue will check type*)
  | Read(lvalue) ->let temp_lvalue_type = get_lvalue_type(lvalue) in 
  	let temp_lvalue_stack_num = (get_lvalue_stack_num lvalue) in (*ref ?*)
	  	if(temp_lvalue_type = Int)
	  	then 
	  		if (get_lvalue_ref_or_not lvalue)(* is ref *)
	  		then (print_read_int;
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_int;
	  					print_store temp_lvalue_stack_num "r0")
	  	else 
	  		if (get_lvalue_ref_or_not lvalue)(* is ref *)
	  		then (print_read_bool; (*result is in r0 *)
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_int;
	  					print_store temp_lvalue_stack_num "r0")
  | Write(expr) -> if check_expr_type expr 
  	then 
  		if(cur_expr_type = Int) (*write int, result of arithematic is in r0*)
  		then 
  			(cur_func_symbol_hash_table := hash_table;
  				codegen_arithmatic expr;
  				print_print_int)
  		else 
  			( match cur_expr_type with (*write string*)
  				| IdentType(string_temp) -> (print_string_const (get_register_string 0) string_temp;
  					print_print_string
  				| _ -> (Printf.printf "type error when writing a string\n";exit 0)
  				
		else (Printf.printf "write check type failed \n" ; exit0)
  | Method(method_name, paramList) ->(register_count := 0;
  	print_call method_name;
  	List.iter (process_calling_method_param (get_hash_table_symbol (Hashtbl.find symbol_table_hash method_name)) ) paramList)
  | WhileDec(expr, stmt_list) -> (print_label_by_number cur_label_count;
  	let while_label = (!cur_label_count) in 
	  	let while_out_label = (!cur_label_count)+1 in 
		  	(cur_label_count := cur_label_count + 2;
		  		codegen_arithmatic expr; (*compare condition *)
			  	print_branch_on_false "r0" (get_label_name while_label); (*branch is different to call .*)
			  	List.iter (codegen_one_stmt hash_table) stmt_list; (**)
			  	print_branch_unc (get_label_name while_out_label);
			  	print_label_by_number while_out_label;)
	(*will reserve else label as well, eventhough it might have no else part*)
  | IfDec(expr,then_stmt_list,else_stmt_list) -> let if_else_label = (!cur_label_count) in
  	let if_out_label= (!cur_label_count)+1 in ( cur_label_count:=(!cur_label_count)+2;
  		if(List.length (else_stmt_list))!= 0 (*if have else and condition is flase then go to else*)
  		then (cur_register_count := 0;
  			codegen_arithmatic expr;
  			print_branch_on_false "r0" (get_label_name if_else_label);
  			List.iter (codegen_one_stmt hash_table) then_stmt_list ;
  			print_branch_unc (get_label_name if_out_label);
  			print_label_by_number if_else_label;
  			List.iter (codegen_one_stmt hash_table) else_stmt_list)(*if false go to else label, if true say in and jump uncondition to out label*)
  			print_label_by_number if_out_label;
  		else (print_branch_on_false "r0" (get_label_name if_out_label);(*if not else and false go to out label*)
  			print_branch_unc (get_label_name if_out_label);
  			List.iter (codegen_one_stmt hash_table) then_stmt_list;
  			print_label_by_number if_out_label)(* if not else directly go *) ))
  | _ -> (Printf.printf "start_translate_by_function_stmt_list error \n";exit 0)



let check_if_ref func_name key = (*see is var ref or not*)

let check_one_stmt_type stmt_data type_data = (*write, read, assign, *)

let check_valid_arguments func_name method_data = 

let start_translate_by_function_declaration func_name = match one_functionDeclaration with
	|(func_name,funcDecParamList) -> (register_count := -1;
		print_label_by_function_name function_name;
		try let function_symbol_table_hash = Hashtbl.find symbol_table_hash func_name in
			(print_push_stack_frame (Hashtbl.find func_stack_num_hash func_name);
				List.iter (fun x -> (register_count := register_count + 1;
					codegen_param_init function_symbol_table_hash hash_table x)) funcDecParamList;
				register_count := 0)
		with Not_found -> (Printf.printf "%s symbol_table_hash not found\n" ; exit 0))

let rec start_translate_by_function_variable_declaration func_name typedefStruct_list = let var_init_symbol_table = Hashtbl.find symbol_table_hash func_name in
	(print_int_const register_count 0; (*initialize everything to 0*)
		List.iter (codegen_var_init) typedefStruct_list)

let start_translate_by_function_stmt_list func_name stmt_list = let temp_hash_symbol_table = Hashtbl.find symbol_table_hash func_name in
	(cur_expr_type := IdentType;
		cur_func_symbol_hash_table := (Hashtbl.find symbol_table_hash func_name);
		List.iter (codegen_one_stmt temp_hash_symbol_table) stmt_list)


let start_translate_by_function one_funcdef = match one_funcdef with
	|((func_name,_),typedefStruct_list,stmt_list) ->(start_translate_by_function_declaration func_name;
		start_translate_by_function_variable_declaration func_name typedefStruct_list;
		start_translate_by_function_stmt_list func_name stmtList;
		print_return)

let rec find_main_funcdef funcdefs = match funcdefs with
	| (((fun_name,_),_,_) as temp::rest_funcdefs) -> if fun_name = "main" then temp else find_main_funcdef rest_funcdefs
	| [] -> (Printf.printf "No main method present\n";exit 0)


let start_test_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs);

		Printf.printf "----- Start Printing typdef_table_hash -----\n";
		print_out_one_typedef_table typdef_table_hash;
		Printf.printf "----- End Printing typdef_table_hash -----\n";

		Printf.printf "----- Start Printing symbol_table_hash -----\n";
		print_out_one_symbol_table symbol_table_hash;
		Printf.printf "----- End Printing symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_param_symbol_table_hash -----\n";
		print_out_one_symbol_table func_param_symbol_table_hash;
		Printf.printf "----- End Printing func_param_symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_stack_num_hash -----\n";
		print_func_stack_num_hash func_stack_num_hash;
		Printf.printf "----- End Printing func_stack_num_hash -----\n";


		find_main_funcdef (prog.funcdefs);
		Printf.printf "----- Start Generating Oz Code -----\n";
		print_call "main";
		print_halt;



		Printf.printf "----- End Generating Oz Code -----\n")

let start_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs))
		(*print_out_one_typedef_table typdef_table_hash;*)
		(*print_out_one_symbol_table symbol_table_hash);*)
		(*translate main first *)

