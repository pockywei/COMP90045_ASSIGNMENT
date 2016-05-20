val t_flag : bool ref
val t0_free : bool ref

val get_register_string : Bean_ast.stackNum -> Bean_ast.ident
val print_push_stack_frame : Bean_ast.stackNum -> unit
val print_pop_stack_frame : Bean_ast.stackNum -> unit
val print_load : Bean_ast.ident -> Bean_ast.stackNum -> unit
val print_store : Bean_ast.stackNum -> Bean_ast.ident -> unit
val print_load_address : Bean_ast.ident -> Bean_ast.stackNum -> unit
val print_load_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
val print_store_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
val print_int_const : Bean_ast.ident -> Bean_ast.stackNum -> unit
val print_string_const : Bean_ast.ident -> Bean_ast.ident -> unit
val print_add_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_add_offset : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_sub_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_sub_offset : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_mul_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_div_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_eq_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_ne_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_gt_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_ge_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_lt_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_cmp_le_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_and : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_or : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
val print_not : Bean_ast.ident -> Bean_ast.ident -> unit
val print_move : Bean_ast.ident -> Bean_ast.ident -> unit
val print_call : Bean_ast.ident -> unit
val call_builtin : 'a -> Bean_ast.ident -> unit
val print_branch_on_true : Bean_ast.ident -> Bean_ast.ident -> unit
val print_branch_on_false : Bean_ast.ident -> Bean_ast.ident -> unit
val print_branch_on_unc : Bean_ast.ident -> unit
val print_label_by_number : Bean_ast.stackNum -> unit
val print_label_by_function_name : Bean_ast.ident -> unit
val get_label_name : Bean_ast.stackNum -> Bean_ast.ident
val print_return : unit -> unit
val print_halt : unit -> unit
val print_debug_reg : Bean_ast.ident -> unit
val print_debug_slot : Bean_ast.stackNum -> unit
val print_debug_stack : unit -> unit
val print_read_int : unit -> unit
val print_read_bool : unit -> unit
val print_print_int : unit -> unit
val print_print_bool : unit -> unit
val print_print_string : unit -> unit
val codegen_binop : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> Bean_ast.binop -> unit
val printBinop : Bean_ast.binop -> Bean_ast.ident
val codegen_unop : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.unop -> unit
val getStackNum : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.stackNum
val get_lvalue_stack_num : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.stackNum
val codegen_arithmatic : Bean_ast.expr -> Bean_ast.stackNum
