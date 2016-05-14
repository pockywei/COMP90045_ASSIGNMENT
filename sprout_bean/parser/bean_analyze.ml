open Bean_ast
open Bean_symbol
open Bean_codegen

let start_test_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs);
		print_out_one_typedef_table typdef_table_hash;
		print_out_one_symbol_table symbol_table_hash)


