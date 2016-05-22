(** The is the ast file contains the methods for code semantic analysing
 *
 *
 *
 * Program Description : This program is for the project of COMP90045 
 * at the University of Melbourne,
 * it is a compiler program for the bean language
 *
 * Team Member : 
 * Angus Huang 640386
 * Bingfeng Liu 639187
 * Chesdametrey Seng 748852
 * Chenhao Wei 803931
 *
 * Project Created Date : 18.03.2016
 *)
(* keep track of register count*)
val register_count : Bean_ast.stackNum ref
(* is current param Ref?*)
val cur_param_ref : bool ref
(* get current LId from lvaue*)
val get_cur_LId : Bean_ast.lvalue -> Bean_ast.ident
(* get the rest of lvalue*)
val get_rest_lvalue : Bean_ast.lvalue -> Bean_ast.lvalue
(* try to generate code for rvalue*)
val process_rvalue : bool -> Bean_ast.lvalue -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.rvalue -> unit
(* get the beantype from the symbol type struct*)
val get_symbol_hash_table_primitive_type : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.beantype
(* matching type of expr with curren expr*)
val expr_type_match_with_cur_expr_type : Bean_ast.beantype -> bool
(* get type from lvalue*)
val get_lvalue_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.beantype
(* check expr type*)
val check_expr_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.expr -> bool
(* get variable stack number from symbol type struct*)
val getStackNum : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.stackNum
(* check symbol table struct is Ref or Val from hash table*)
val get_bool_ref_val_symbol_hash_table : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> bool
(* check symbol table struct is Ref or Val*)
val get_bool_ref_val_symbol_type : Bean_ast.symbolTableType -> bool
(* check lavalue is Ref or not*)
val get_lvalue_ref_or_not : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> bool
(* get symbol table struct from lvalue*)
val get_lvalue_symbol_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.symbolTableType
(* get stack number from lvalue*)
val get_lvalue_stack_num : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.stackNum
(* add LId to the last of lvalue*)
val add_in_Lid_to_Last : Bean_ast.lvalue -> string -> Bean_ast.lvalue
(* generate code for variable initialization*)
val codegen_var_init : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct -> unit
(* code generation fo variable initialization*)
val codegen_var_init_incr_ver : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct -> unit
(* code generation for parameters*)
val codegen_param_init : (string, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.valRef * Bean_ast.typedefStruct * string -> unit
(* check symbol struct equality regardless Val or Ref*)
val check_ref_val_type_equal : Bean_ast.symbolTableType -> Bean_ast.symbolTableType -> bool
(* converting an expr to symbl type struct*)
val convert_one_expr_param_to_symbol_type : Bean_ast.expr -> Bean_ast.symbolTableType
(* use symbol type struct to get corresponding stack num*)
val gen_symbol_type_to_register : Bean_ast.symbolTableType -> bool -> bool -> unit
(* code generating for calling a method*)
val process_calling_method_param : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.expr -> Bean_ast.valRef * 'a * Bean_ast.ident -> Bean_ast.stackNum
(* co generation for each statments in the method body*)
val codegen_one_stmt : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.stmt -> unit
(* start code translation the function declaration *)
val start_translate_by_function_declaration : 'a -> Bean_ast.ident * (Bean_ast.valRef * Bean_ast.typedefStruct * Bean_ast.ident) list -> unit
(* start code tranlation for variable initialization*)
val start_translate_by_function_variable_declaration : Bean_ast.ident -> Bean_ast.typedefStruct list -> unit
(* start code translation for method statment body*)
val start_translate_by_function_stmt_list : Bean_ast.ident -> Bean_ast.stmt list -> unit
(* start code translation function by function*)
val start_translate_by_function : (Bean_ast.ident * (Bean_ast.valRef * Bean_ast.typedefStruct * Bean_ast.ident) list) * Bean_ast.typedefStruct list * Bean_ast.stmt list -> unit
(* funding function structs according to function name*)
val find_funcdef : (('a * 'b) * 'c * 'd) list -> 'a -> ('a * 'b) * 'c * 'd
(* start analyzing*)
val start_test_analyzer : Bean_ast.t -> unit
(* start analyzing code*)
val start_analyzer : Bean_ast.t -> unit
(* generate code for store rvalue to lvalue*)
val codegen_store_rvalue : Bean_ast.symbolTableType -> Bean_ast.symbolTableType -> bool -> unit
(* get primitive struct types's stack num*)
val get_symbol_hash_table_primitive_type_stack_num : Bean_ast.symbolTableType -> Bean_ast.stackNum
(* generate all the fields initialization for typedef*)
val codgen_all_param_fields : Bean_ast.symbolTableType -> (string, Bean_ast.symbolTableType) Hashtbl.t -> unit