val register_count : Bean_ast.stackNum ref
val cur_param_ref : bool ref
val get_cur_LId : Bean_ast.lvalue -> Bean_ast.ident
val get_rest_lvalue : Bean_ast.lvalue -> Bean_ast.lvalue
val process_rvalue : bool -> Bean_ast.lvalue -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.rvalue -> unit
val get_symbol_hash_table_primitive_type : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.beantype
val expr_type_match_with_cur_expr_type : Bean_ast.beantype -> bool
val get_lvalue_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.beantype
val check_expr_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.expr -> bool
val getStackNum : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.stackNum
val get_bool_ref_val_symbol_hash_table : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> bool
val get_bool_ref_val_symbol_type : Bean_ast.symbolTableType -> bool
val get_lvalue_ref_or_not : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> bool
val get_lvalue_symbol_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.symbolTableType
val get_lvalue_stack_num : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.stackNum
val add_in_Lid_to_Last : Bean_ast.lvalue -> string -> Bean_ast.lvalue
val codegen_var_init : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct -> unit
val codegen_var_init_incr_ver : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct -> unit
val codegen_param_init : (string, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.valRef * Bean_ast.typedefStruct * string -> unit
val check_ref_val_type_equal : Bean_ast.symbolTableType -> Bean_ast.symbolTableType -> bool
val convert_one_expr_param_to_symbol_type : Bean_ast.expr -> Bean_ast.symbolTableType
val gen_symbol_type_to_register : Bean_ast.symbolTableType -> bool -> bool -> unit
val process_calling_method_param : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.expr -> Bean_ast.valRef * 'a * Bean_ast.ident -> Bean_ast.stackNum
val codegen_one_stmt : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.stmt -> unit
val start_translate_by_function_declaration : 'a -> Bean_ast.ident * (Bean_ast.valRef * Bean_ast.typedefStruct * Bean_ast.ident) list -> unit
val start_translate_by_function_variable_declaration : Bean_ast.ident -> Bean_ast.typedefStruct list -> unit
val start_translate_by_function_stmt_list : Bean_ast.ident -> Bean_ast.stmt list -> unit
val start_translate_by_function : (Bean_ast.ident * (Bean_ast.valRef * Bean_ast.typedefStruct * Bean_ast.ident) list) * Bean_ast.typedefStruct list * Bean_ast.stmt list -> unit
val find_funcdef : (('a * 'b) * 'c * 'd) list -> 'a -> ('a * 'b) * 'c * 'd
val start_test_analyzer : Bean_ast.t -> unit
val start_analyzer : Bean_ast.t -> unit
val codegen_store_rvalue : Bean_ast.symbolTableType -> Bean_ast.symbolTableType -> bool -> unit
val get_symbol_hash_table_primitive_type_stack_num : Bean_ast.symbolTableType -> Bean_ast.stackNum

val codgen_all_param_fields : Bean_ast.symbolTableType -> (string, Bean_ast.symbolTableType) Hashtbl.t -> unit