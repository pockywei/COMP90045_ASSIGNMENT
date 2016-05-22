(** The is the bean_symbol contains will build and store symbol table for type
 * checking and stack num tracking. The hash is used as the datascture for the
 * symbol table for fast look up.
 *  
 *
 * Description : This program is for the project of COMP90045 
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
(* record current register used *)
val cur_register_count : Bean_ast.stackNum ref
(* what the overall expression type should be *)
val top_level_expr_type : Bean_ast.beantype ref
(* current expression type *)
val cur_expr_type : Bean_ast.beantype ref
(* current label number used  *)
val cur_label_count : Bean_ast.stackNum ref
(* start with 20 slot in hash table *)
val hash_table_size : Bean_ast.stackNum
(* keep count of stack *)
val stack_count : Bean_ast.stackNum ref
(* variable symbol table *)
val symbol_table_hash : (string, Bean_ast.symbolTableType) Hashtbl.t
(* typedef symbol table *)
val typdef_table_hash : (string, Bean_ast.typedefTableType) Hashtbl.t
(* function total stack num hash table *)
val func_stack_num_hash : (string, int) Hashtbl.t
(* function parameters hashtable *)
val func_param_symbol_table_hash : (string, (Bean_ast.functionDeclaration*Bean_ast.typedefStruct list*Bean_ast.stmt list)) Hashtbl.t
(* current symbol table used *)
val cur_func_symbol_hash_table : (string, Bean_ast.symbolTableType) Hashtbl.t ref
(*  ordered function parameter table *)
val func_param_order_hash_table : (string, (Bean_ast.functionDeclaration*Bean_ast.typedefStruct list*Bean_ast.stmt list)) Hashtbl.t
(*  record stack num*)
val symbol_table_stackNum_hash : (string, Bean_ast.stackNum) Hashtbl.t
(*  get one typedef symbol table back *)
val get_hash_table_typedef : Bean_ast.typedefTableType -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t
(*  get one symbol hash table back from symbol table struct *)
val get_hash_table_symbol : Bean_ast.symbolTableType -> (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t
(*  build one typedef symbol table helper method *)
val build_one_typedef_table_hash_ : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
(*  buld one typeef symbol table*)
val build_one_typedef_table_hash : Bean_ast.typedefStruct * Bean_ast.ident -> unit
(*  build entire typedef symbol table *)
val build_typedef_table_hash : (Bean_ast.typedefStruct * Bean_ast.ident) list -> unit
(*  build symbol table with self defined struct might have many fields*)
val build_symbol_table_self_type : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
(*  build symbol table with suitable Ref symbol struct *)
val build_symbol_table_self_type_ref : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
(*  use typedef sturct list (variable initialization) to build symboltable *)
val build_symbol_table_typedefStruct_list : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.typedefStruct list -> unit
(* build variable symbol table *)
val build_symbol_table_hash_funcDecParamList : ('a, Bean_ast.symbolTableType) Hashtbl.t -> (Bean_ast.valRef * Bean_ast.typedefStruct * 'a) list -> unit
(* build all hash table symbol table *)
val build_symbol_table_hash_all : ((string *(Bean_ast.valRef * Bean_ast.typedefStruct * string) list) *Bean_ast.typedefStruct list * Bean_ast.stmt list) list -> unit
(* debug used print out total stack num of function *)
val print_func_stack_num_hash : (Bean_ast.ident, Bean_ast.stackNum) Hashtbl.t -> unit
(* debug used print out one symbol table *)
val print_out_one_symbol_table : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> unit
(* debug used print typedef symbol table *)
val print_out_one_typedef_table : (Bean_ast.ident, Bean_ast.typedefTableType) Hashtbl.t -> unit
