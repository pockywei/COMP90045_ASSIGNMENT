val t_flag : bool ref
val t0_free : bool ref
val cur_register_count : int ref
val cur_label_count : int ref
val printBinop : Bean_ast.binop -> bytes
val codegen_arithmatic : Bean_ast.expr -> int
val codegen_stmts : 'a * 'b * Bean_ast.stmt list -> unit
val inc_cur_register_count : unit -> unit