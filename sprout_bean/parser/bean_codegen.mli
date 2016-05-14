val t_flag : bool ref
val t0_free : bool ref
val cur_register_count : int ref
val cur_label_count : int ref
val print_push_stack_frame : int -> unit
val print_pop_stack_frame : int -> unit
val print_load : Bean_ast.ident -> int -> unit
val print_sotre : int -> Bean_ast.ident -> unit
val print_load_address : Bean_ast.ident -> int -> unit
val print_load_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
val print_store_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
val print_int_const : Bean_ast.ident -> int -> unit
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