val hash_table_size : int
val stack_count : int ref
val symbol_table_hash : ('_a, '_b) Hashtbl.t
val typdef_table_hash : ('_a, '_b) Hashtbl.t
val func_stack_num_hash : ('_a, '_b) Hashtbl.t
val func_param_symbol_table_hash : ('_a, '_b) Hashtbl.t
val cur_func_symbol_hash_table : ('_a, '_b) Hashtbl.t ref
val func_param_order_hash_table : ('_a, '_b) Hashtbl.t
val symbol_table_stackNum_hash : ('_a, '_b) Hashtbl.t
val get_hash_table_typedef : Bean_ast.typedefTableType -> (bytes, Bean_ast.typedefTableType) Hashtbl.t
val get_hash_table_symbol : Bean_ast.symbolTableType -> (bytes, Bean_ast.symbolTableType) Hashtbl.t
val build_one_typedef_table_hash_ : (bytes, Bean_ast.typedefTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_one_typedef_table_hash : Bean_ast.typedefStruct * bytes -> unit
val build_typedef_table_hash : (Bean_ast.typedefStruct * bytes) list -> unit
val build_symbol_table_self_type : (bytes, Bean_ast.symbolTableType) Hashtbl.t -> (bytes, Bean_ast.typedefTableType) Hashtbl.t -> unit
val build_symbol_table_typedefStruct_list : (bytes, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_symbol_table_hash_funcDecParamList : ('a, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.valRef * Bean_ast.typedefStruct * 'a) list -> unit
val build_symbol_table_hash_all : (('_a * (Bean_ast.valRef * Bean_ast.typedefStruct * bytes) list) * Bean_ast.typedefStruct list * '_b) list -> unit
val print_func_stack_num_hash : (bytes, int) Hashtbl.t -> unit
val print_out_one_symbol_table : (bytes, Bean_ast.symbolTableType) Hashtbl.t -> unit
val print_out_one_typedef_table : (bytes, Bean_ast.typedefTableType) Hashtbl.t -> unit