open Bean_ast
open Bean_symbol
open Bean_codegen

let register_count = ref -1
let cur_expr_type = ref Enone

let expr_type_match_with_cur_expr_type bean_type

let get_expr_type hash_table expr = match expr with
	| Ebool(bool_val) -> 
  | Eint(int_val) ->
  | Elval(lvalue) ->
  | Ebinop(expr,binop,expr) ->
  | Eunop(unop,expr) ->
  | Eident(ident) ->
  | Ebracket(expr) ->

let getStackNum hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  | S_Struct(_ , stackNum) -> stackNum
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ , stackNum) -> stackNum
  | _ -> (Printf.printf "get stack num error\n"; exit 0)

let get_lvalue_type hash_table lvalue = match lvalue with
	| LId(ident) -> get_hash_table_symbol(Hashtbl.find hash_table ident)
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

let rec codegen_one_stmt hash_Table one_stmt = match one_stmt with
	| Assign of (lvalue, rvalue) -> 
  | Read(lvalue) ->let temp_lvalue_type = get_lvalue_type(lvalue) in 
  	if(temp_lvalue_type = Int)
  	then print_read_int
  	else 
  		if (temp_lvalue_type = Bool)
  		then print_read_bool
  		else (Printf.printf "Read type error\n"; exit 0 )
  | Write(expr) ->
  | Method(method_name, paramList) ->
  | WhileDec(expr, stmt_list) ->
  | IfDec(expr,then_stmt_list,else_stmt_list) ->
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

let start_translate_by_function_stmt_list func_name stmtList = let temp_symbol


let start_translate_by_function one_funcdef = match one_funcdef with
	|((func_name,_),typedefStruct_list,stmt_list) ->(start_translate_by_function_declaration func_name;
		start_translate_by_function_variable_declaration func_name typedefStruct_list;
		start_translate_by_function_stmt_list func_name stmtList)

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

