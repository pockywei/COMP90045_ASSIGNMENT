val cur_register_count : Bean_ast.stackNum ref
val top_level_expr_type : Bean_ast.beantype ref
val cur_expr_type : Bean_ast.beantype ref
val cur_label_count : Bean_ast.stackNum ref
val hash_table_size : Bean_ast.stackNum
val stack_count : Bean_ast.stackNum ref
val symbol_table_hash : (string, Bean_ast.symbolTableType) Hashtbl.t
val typdef_table_hash : (string, Bean_ast.typedefTableType) Hashtbl.t
val func_stack_num_hash : (string, int) Hashtbl.t
val func_param_symbol_table_hash : (string, (Bean_ast.functionDeclaration*Bean_ast.typedefStruct list*Bean_ast.stmt list)) Hashtbl.t
val cur_func_symbol_hash_table : (string, Bean_ast.symbolTableType) Hashtbl.t ref
val func_param_order_hash_table : (string, (Bean_ast.functionDeclaration*Bean_ast.typedefStruct list*Bean_ast.stmt list)) Hashtbl.t
val symbol_table_stackNum_hash : (string, Bean_ast.stackNum) Hashtbl.t
val get_hash_table_typedef : Bean_ast.typedefTableType -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t
val get_hash_table_symbol : Bean_ast.symbolTableType -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t
val build_one_typedef_table_hash_ : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_one_typedef_table_hash : Bean_ast.typedefStruct * Bean_ast.ident -> unit
val build_typedef_table_hash : (Bean_ast.typedefStruct * Bean_ast.ident) list -> unit
val build_symbol_table_self_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
val build_symbol_table_typedefStruct_list : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
val build_symbol_table_hash_funcDecParamList : ('a, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.valRef * Bean_ast.typedefStruct * 'a) list -> unit
val build_symbol_table_hash_all : ((string *(Bean_ast.valRef * Bean_ast.typedefStruct * string) list) *Bean_ast.typedefStruct list * Bean_ast.stmt list) list -> unit
val print_func_stack_num_hash : (Bean_ast.ident, Bean_ast.stackNum) Hashtbl.t -> unit
val print_out_one_symbol_table : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> unit
val print_out_one_typedef_table : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
