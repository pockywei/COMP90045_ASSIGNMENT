val t_flag : bool ref
val t0_free : bool ref
val cur_register_count : int ref
val cur_label_count : int ref
val print_push_stack_frame : int -> unit
val print_pop_stack_frame : int -> unit
val print_load : bytes -> int -> unit
val print_store : int -> bytes -> unit
val print_load_address : bytes -> int -> unit
val print_load_indirect : bytes -> bytes -> unit
val print_store_indirect : bytes -> bytes -> unit
val print_int_const : bytes -> int -> unit
val print_string_const : bytes -> bytes -> unit
val print_add_int : bytes -> bytes -> bytes -> unit
val print_add_offset : bytes -> bytes -> bytes -> unit
val print_sub_int : bytes -> bytes -> bytes -> unit
val print_sub_offset : bytes -> bytes -> bytes -> unit
val print_mul_int : bytes -> bytes -> bytes -> unit
val print_div_int : bytes -> bytes -> bytes -> unit
val print_cmp_eq_int : bytes -> bytes -> bytes -> unit
val print_cmp_ne_int : bytes -> bytes -> bytes -> unit
val print_cmp_gt_int : bytes -> bytes -> bytes -> unit
val print_cmp_ge_int : bytes -> bytes -> bytes -> unit
val print_cmp_lt_int : bytes -> bytes -> bytes -> unit
val print_cmp_le_int : bytes -> bytes -> bytes -> unit
val print_and : bytes -> bytes -> bytes -> unit
val print_or : bytes -> bytes -> bytes -> unit
val print_not : bytes -> bytes -> unit
val print_move : bytes -> bytes -> unit
val print_call : bytes -> unit
val call_builtin : 'a -> bytes -> unit
val print_branch_on_true : bytes -> bytes -> unit
val print_branch_on_false : bytes -> bytes -> unit
val print_label_by_number : int -> unit
val print_return : unit -> unit
val print_halt : unit -> unit
val print_debug_reg : bytes -> unit
val print_debug_slot : int -> unit
val print_debug_stack : unit -> unit
val print_label_by_function_name : bytes -> unit
val print_read_int : unit -> unit
val print_read_bool : unit -> unit
val print_print_int : bytes -> unit
val print_print_bool : bytes -> unit
val print_print_string : bytes -> unit
val printBinop : Bean_ast.binop -> bytes
val codegen_stmts : 'a * 'b * Bean_ast.stmt list -> unit
val inc_cur_register_count : unit -> unit
