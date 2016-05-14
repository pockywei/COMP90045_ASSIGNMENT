open Bean_ast
open Bean_symbol
open Bean_codegen
(*

let start_translate_by_function_declaration functionDeclaration =

let start_translate_by_function_variable_declaration typedefStruct =

let start_translate_by_function_stmt_list stmtList =

let start_translate_by_function one_funcdefs = match one_funcdefs with
	|(functionDeclaration,typedefStruct_list,stmt_list) ->(start_translate_by_function_declaration functionDeclaration;
		start_translate_by_function_variable_declaration typedefStruct;
		start_translate_by_function_stmt_list stmtList)
*)
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

		print_func_stack_num_hash func_stack_num_hash;
		find_main_funcdef (prog.funcdefs);
		Printf.printf "----- Start Generating Oz Code -----\n";
		(*print_call "main";*)
		(*print_halt;*)



		Printf.printf "----- End Generating Oz Code -----\n")

let start_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs))
		(*print_out_one_typedef_table typdef_table_hash;*)
		(*print_out_one_symbol_table symbol_table_hash);*)
		(*translate main first *)

