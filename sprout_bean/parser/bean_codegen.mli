(** 
 *
 * Description : This program is for the project of COMP90045 
 * at the University of Melbourne, it is a compiler program for the bean language
 *
 * The is the codegen file contains the methods to print out all the
 * Oz instructions 
 *
 * Team Member : 
 * Angus Huang 640386
 * Bingfeng Liu 639187
 * Chesdametrey Seng 748852
 * Chenhao Wei 803931
 *
 * Project Created Date : 18.03.2016
 *)

(* generate register name string => "rX" *)
val get_register_string : Bean_ast.stackNum -> Bean_ast.ident
(* print push stack frame instruction *)
val print_push_stack_frame : Bean_ast.stackNum -> unit
(* print pop stack frame instruction *)
val print_pop_stack_frame : Bean_ast.stackNum -> unit
(* print load from stack instruction *)
val print_load : Bean_ast.ident -> Bean_ast.stackNum -> unit
(* print store to stack instruction *)
val print_store : Bean_ast.stackNum -> Bean_ast.ident -> unit
(* print load address from stack instruction *)
val print_load_address : Bean_ast.ident -> Bean_ast.stackNum -> unit
(* print de-reference the address and update its value instruction *)
val print_load_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
(* print update the value of the address stored in the stack instruction *)
val print_store_indirect : Bean_ast.ident -> Bean_ast.ident -> unit
(* print load int constant into register instruction *)
val print_int_const : Bean_ast.ident -> Bean_ast.stackNum -> unit
(* print load string constant into register instruction *)
val print_string_const : Bean_ast.ident -> Bean_ast.ident -> unit
(* print add int instruction *)
val print_add_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print add address offset instruction *)
val print_add_offset : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print sub int instruction *)
val print_sub_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print sub address offset instruction *)
val print_sub_offset : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print mul int instruction *)
val print_mul_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print div int instruction *)
val print_div_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp equality int instruction *)
val print_cmp_eq_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp not equality int instruction *)
val print_cmp_ne_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp greater int instruction *)
val print_cmp_gt_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp greater and equal int instruction *)
val print_cmp_ge_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp less than instruction *)
val print_cmp_lt_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print cmp less than and equal instruction *)
val print_cmp_le_int : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print and logic operator instruction *)
val print_and : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print or logic operator instruction *)
val print_or : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> unit
(* print not negation operator instruction *)
val print_not : Bean_ast.ident -> Bean_ast.ident -> unit
(* print move => swap int instruction *)
val print_move : Bean_ast.ident -> Bean_ast.ident -> unit
(* print call label instruction *)
val print_call : Bean_ast.ident -> unit
(* print call builtin function instruction *)
val call_builtin : 'a -> Bean_ast.ident -> unit
(* print branch true instruction *)
val print_branch_on_true : Bean_ast.ident -> Bean_ast.ident -> unit
(* print branch false instruction *)
val print_branch_on_false : Bean_ast.ident -> Bean_ast.ident -> unit
(* print branch unconditoin instruction *)
val print_branch_on_unc : Bean_ast.ident -> unit
(* print label_X  *)
val print_label_by_number : Bean_ast.stackNum -> unit
(* print function name as label  *)
val print_label_by_function_name : Bean_ast.ident -> unit
(* print return a label name  *)
val get_label_name : Bean_ast.stackNum -> Bean_ast.ident
(* print return instruction *)
val print_return : unit -> unit
(* print halt instruction *)
val print_halt : unit -> unit
(* print debug_reg instruction *)
val print_debug_reg : Bean_ast.ident -> unit
(* print debug_slot instruction *)
val print_debug_slot : Bean_ast.stackNum -> unit
(* print debug_stack instruction *)
val print_debug_stack : unit -> unit
(* print read_int instruction *)
val print_read_int : unit -> unit
(* print read_bool instruction *)
val print_read_bool : unit -> unit
(* print print int instruction *)
val print_print_int : unit -> unit
(* print print bool instruction *)
val print_print_bool : unit -> unit
(* print print string instruction *)
val print_print_string : unit -> unit
(* print corresponding binop Oz instruction *)
val codegen_binop : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.ident -> Bean_ast.binop -> unit
(* print binop for debug instruction *)
val printBinop : Bean_ast.binop -> Bean_ast.ident
(* print generate unop Oz instruction *)
val codegen_unop : Bean_ast.ident -> Bean_ast.ident -> Bean_ast.unop -> unit
(* get current variable's stack num instruction *)
val getStackNum : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> Bean_ast.stackNum
(* get lvalue's stack num instruction *)
val get_lvalue_stack_num : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> Bean_ast.stackNum
(* is_Ref? *)
val get_bool_ref_val_symbol_hash_table : ('a, Bean_ast.symbolTableType) Hashtbl.t -> 'a -> bool
(* is_Ref? *)
val get_lvalue_ref_or_not : (Bean_ast.ident, Bean_ast.symbolTableType) Hashtbl.t -> Bean_ast.lvalue -> bool
(* generate arithmatic opeartion's Oz code *)
val codegen_arithmatic : Bean_ast.expr -> Bean_ast.stackNum
(* generate Ref arithmatic operation's Oz Code*)
val codegen_arithmatic_ref : Bean_ast.expr -> Bean_ast.stackNum

