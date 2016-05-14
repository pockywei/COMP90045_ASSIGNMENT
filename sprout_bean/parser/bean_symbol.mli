val hash_table_size : int
val stack_count : int ref
val symbol_table_hash : (string, Bean_ast.symbolTableType) Hashtbl.t
val func_stack_num_hash : (string, int) Hashtbl.t
val typdef_table_hash : (string, Bean_ast.typedefTableType) Hashtbl.t
val func_param_symbol_table_hash : (string, Bean_ast.symbolTableType) Hashtbl.t
(*val symbol_table_int_hash : (string, Bean_ast.symbolTableType) Hashtbl.t*)
val get_hash_table_typedef : Bean_ast.typedefTableType -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t
val get_hash_table_symbol : Bean_ast.symbolTableType -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t
val build_one_typedef_table_hash_ : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_one_typedef_table_hash : Bean_ast.typedefStruct * Bean_ast.ident -> unit
val build_typedef_table_hash : (Bean_ast.typedefStruct * Bean_ast.ident) list -> unit
val build_symbol_table_self_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
val build_symbol_table_hash_funcDecParamList : ('a, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.valRef * Bean_ast.typedefStruct * 'a) list -> unit
val build_symbol_table_typedefStruct_list : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_symbol_table_hash_all : ((Bean_ast.ident * (Bean_ast.valRef * Bean_ast.typedefStruct * Bean_ast.ident) list) * Bean_ast.typedefStruct list * 'a) list -> unit
val print_out_one_symbol_table : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> unit
val print_out_one_typedef_table : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
val print_func_stack_num_hash : (string, int) Hashtbl.t -> unit
