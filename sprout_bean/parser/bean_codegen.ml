open Bean_ast
open Bean_symbol

(* *)
(* == , this compare refernece, = compare content equality*)


let t_flag = ref true
let t0_free = ref true



let get_register_string register_num = "r"^(string_of_int register_num)

let print_push_stack_frame frame_size = Printf.printf "push_stack_frame %d\n" frame_size

let print_pop_stack_frame frame_size = Printf.printf "pop_stack_frame %d\n" frame_size

let print_load register_name slot_num = Printf.printf "load %s, %d\n" register_name slot_num

let print_store slot_num register_name = Printf.printf "store %d, %s\n" slot_num register_name

let print_load_address register_name slot_num = Printf.printf "load_address %s, %d\n" register_name slot_num

let print_load_indirect register_name_1 register_name_2 = Printf.printf "load_indirect %s, %s\n" register_name_1 register_name_2

let print_store_indirect register_name_1 register_name_2 = Printf.printf "store_indirect %s, %s\n" register_name_1 register_name_2

let print_int_const register_name int_const_value = Printf.printf "int_const %s, %d\n" register_name int_const_value

let print_string_const register_name string_const_value = Printf.printf "string_const %s, %s\n" register_name string_const_value

let print_add_int register_1 register_2 register_3 = Printf.printf "add_int %s, %s, %s\n" register_1 register_2 register_3

let print_add_offset register_1 register_2 register_3 = Printf.printf "add_offset %s, %s, %s\n" register_1 register_2 register_3

let print_sub_int register_1 register_2 register_3 = Printf.printf "sub_int %s, %s, %s\n" register_1 register_2 register_3

let print_sub_offset register_1 register_2 register_3 = Printf.printf "sub_offset %s, %s, %s\n" register_1 register_2 register_3

let print_mul_int register_1 register_2 register_3 = Printf.printf "mul_int %s, %s, %s\n" register_1 register_2 register_3

let print_div_int register_1 register_2 register_3 = Printf.printf "div_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_eq_int register_1 register_2 register_3 = Printf.printf "cmp_eq_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_ne_int register_1 register_2 register_3 = Printf.printf "cmp_ne_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_gt_int register_1 register_2 register_3 = Printf.printf "cmp_gt_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_ge_int register_1 register_2 register_3 = Printf.printf "cmp_ge_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_lt_int register_1 register_2 register_3 = Printf.printf "cmp_lt_int %s, %s, %s\n" register_1 register_2 register_3

let print_cmp_le_int register_1 register_2 register_3 = Printf.printf "cmp_le_int %s, %s, %s\n" register_1 register_2 register_3

let print_and register_1 register_2 register_3 = Printf.printf "and %s, %s, %s\n" register_1 register_2 register_3

let print_or register_1 register_2 register_3 = Printf.printf "or %s, %s, %s\n" register_1 register_2 register_3

let print_not register_1 register_2 = Printf.printf "not %s, %s\n" register_1 register_2

let print_move register_1 register_2 = Printf.printf "move %s, %s\n" register_1 register_2

let print_call label_name = Printf.printf "call %s\n" label_name 

let call_builtin builtin_funciton_name = Printf.printf "call_builtin %s\n"

let print_branch_on_true register_name label_name= Printf.printf "branch_on_true %s, %s\n" register_name label_name

let print_branch_on_false register_name label_name= Printf.printf "branch_on_false %s, %s\n" register_name label_name

let print_branch_on_unc label_name = Printf.printf "branch_uncond %s\n" label_name

let print_label_by_number label_number= Printf.printf "label_%d:\n" label_number

let print_label_by_function_name function_name = Printf.printf "%s:\n" function_name 

let get_label_name label_num = "label_"^(string_of_int label_num)

let print_return () = Printf.printf "return\n"

let print_halt () = Printf.printf "halt\n"

let print_debug_reg register_name = Printf.printf "debug_reg %s\n" register_name

let print_debug_slot slot_num = Printf.printf "debug_slot %d\n" slot_num

let print_debug_stack () = Printf.printf "debug_stack\n"


let print_read_int () = Printf.printf "call_builtin read_int\n"

let print_read_bool () =Printf.printf "call_builtin read_bool\n"

let print_print_int () = Printf.printf "call_builtin print_int\n"

let print_print_bool () = Printf.printf "call_builtin print_bool\n"

let print_print_string () = Printf.printf "call_builtin print_string\n"


(*instruction following the call instruction.
The following are all built-in functions: read_int, read_bool, print_int, print_bool, print_string. *)

(*
push_stack_frame framesize
pop_stack_frame framesize
# C analogues:
load rI, slotnum # rI = x
store slotnum, rI # x = rI
load_address rI, slotnum # rI = &x
load_indirect rI, rJ # rI = *rJ
store_indirect rI, rJ # *rI = rJ
int_const rI, intconst
string_const rI, stringconst
add_int rI, rJ, rK # rI = rJ + rK
add_offset rI, rJ, rK # rI = rJ + rK

sub_int rI, rJ, rK # rI = rJ - rK
sub_offset rI, rJ, rK # rI = rJ - rK
mul_int rI, rJ, rK # rI = rJ * rK
div_int rI, rJ, rK # rI = rJ / rK


cmp_eq_int rI, rJ, rK # rI = rJ == rK
cmp_ne_int rI, rJ, rK
cmp_gt_int rI, rJ, rK # etc.
cmp_ge_int rI, rJ, rK
cmp_lt_int rI, rJ, rK
cmp_le_int rI, rJ, rK
and rI, rJ, rK # rI = rJ && rK
or rI, rJ, rK # rI = rJ || rK
not rI, rJ # rI = !rJ
move rI, rJ # rI = rJ


branch_on_true rI, label # if (rI) goto label
branch_on_false rI, label # if (!rI) goto label
branch_uncond label # goto label
call label
call_builtin builtin_function_name
return
halt
debug_reg rI
debug_slot slotnum
debug_stack






*)


let codegen_binop register_1 register_2 register_3 singleBinop = match singleBinop with
	| Op_add -> print_add_int register_1 register_2 register_3
	| Op_sub -> print_sub_int register_1 register_2 register_3
	| Op_mul -> print_mul_int register_1 register_2 register_3
	| Op_div -> print_div_int register_1 register_2 register_3
	| Op_eq ->  (top_level_expr_type := Bool;  print_cmp_eq_int register_1 register_2 register_3) 
	| Op_lt ->  (top_level_expr_type := Bool; print_cmp_lt_int register_1 register_2 register_3 )
	| Op_gt ->  (top_level_expr_type := Bool; print_cmp_gt_int register_1 register_2 register_3 )
	| Op_neq -> (top_level_expr_type := Bool; print_cmp_ne_int register_1 register_2 register_3 )
	| Op_lte -> (top_level_expr_type := Bool; print_cmp_le_int register_1 register_2 register_3 )
	| Op_gte -> (top_level_expr_type := Bool; print_cmp_ge_int register_1 register_2 register_3 )
	| Op_and -> (if !cur_expr_type = Bool then print_and register_1 register_2 register_3 else (Printf.printf "Op_and type error\n";exit 0))
	| Op_or ->  (if !cur_expr_type = Bool then print_or register_1 register_2 register_3 else  (Printf.printf "Op_or type error\n";exit 0))

let printBinop single_binop = match single_binop with
	| Op_add -> " + "
	| Op_sub -> " - "
	| Op_mul -> " * "
	| Op_div -> " / " 
	| Op_eq -> " = "
	| Op_lt -> " < "
	| Op_gt -> " > "
	| Op_neq -> " != "
	| Op_lte -> " <= "
	| Op_gte -> " >= "
	| Op_and -> " and "
	| Op_or -> " or "

let codegen_unop register_1 register_2 single_unop = match single_unop with
	| Op_minus -> (let temp_cur_register_count = (!cur_register_count) + 1 in (*use s new register to store -1 , but dont update the global register count*)
		print_int_const (get_register_string temp_cur_register_count) (-1);
		print_mul_int register_1 register_2 (get_register_string temp_cur_register_count)  )(*anwer will be stored in register_1, temp_register used to store -1*)
	| Op_not -> (if !cur_expr_type = Bool then print_not register_1 register_1 else (Printf.printf "Op_not type error\n";exit 0))

(*
let start_code_gen one_funcdefs = match one_funcdefs with
	|((func_name,funcDecParamList),typedefStruct_list,stmt_list) -> (Printf.printf "proc_%s:" func_name)

let gen_label gen_label_flag label_num = if gen_label_flag then Printf.printf "label_%d"
*)
(*
let codegen_list_stmts stmts gen_label_flag label_num = 
*)
(*
let dummyIfBlock if_dec after_label = let local_label_count = !cur_label_count in match if_dec with
	|IfDec(expr,thenStmtlist,elseStmtList) -> (cur_label_count := !cur_register_count+1;
		Printf.printf "if t/f:";
		Printf.printf "if false GOTO label_%d" after_label;
		(* call gen stmt_list set a label process elseStmtList part*)
		Printf.printf  "rest statement")
		(* call gen stmt_list to do thenStmtList part*)
*)

(*
let dummyWhileBlock while_dec after_label= let local_label_count = !cur_label_count in match while_dec with
	|WhileDec (expr,stmt_list) -> (cur_label_count := !cur_label_count+1;
		Printf.printf "label_%d" !cur_label_count;
		Printf.printf "isf t/f: ";
		Printf.printf "if false GOTO label_%d" after_label;
		Printf.printf "label_%d" !cur_label_count;
		(*call gen stmt_list stmt_list*)
		(*dummyWhileBlock stmt_list local_label_count*)
		Printf.printf "GOTO label_%d" !cur_label_count)
*)

let rec getStackNum hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  (*| S_Struct(_ , stackNum) -> stackNum*)
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ , stackNum) -> stackNum
  | _ -> (Printf.printf "get stack num error\n"; exit 0)

 let rec get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table =  get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_stack_num temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on get stack num lvalue type \n"; exit 0)


(*true => ref, false => val *)
let get_bool_ref_val_symbol_hash_table hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , _) -> false
  | S_Int(_ , _) -> false
  | S_Hash(_,_) -> false
  | S_Intext_Hash(_) -> false
  | S_Ref_Hash(_,_) -> true
  | S_Ref_Int(_ , _) -> true
  | S_Ref_Bool(_ , _) -> true (*only return Bool or Int, typedef of {} type will cause error*)
  | S_Ref_Intext_Hash(_) -> true
  | _ -> (Printf.printf "type error for is ref \n";exit 0)


let rec get_lvalue_ref_or_not hash_table key_name = match key_name with
	| LId(ident) -> get_bool_ref_val_symbol_hash_table hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_ref_or_not temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on checking lvalue type \n"; exit 0)



(*type checked before executing this line*)
let rec codegen_arithmatic expr = let local_register_count = !cur_register_count in match expr with
	| Ebool(false) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) 0;
		local_register_count)
	| Ebool(true) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) 1;
		local_register_count)
	| Eint(int_val) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) int_val;
		local_register_count)
	| Ebinop(expr_one,binop,expr_two) -> let left = codegen_arithmatic expr_one in
		let right = codegen_arithmatic expr_two in
			(decr cur_register_count; (*decrease register used*)
			codegen_binop (get_register_string local_register_count) (get_register_string left) (get_register_string right) binop;
			local_register_count)
	| Eunop(Op_minus,expr) -> (print_int_const (get_register_string local_register_count) (-1);
		print_mul_int (get_register_string local_register_count) (get_register_string local_register_count) (get_register_string(codegen_arithmatic expr));
		local_register_count) (*codegen_Arithmatic expr alone in here cause warnning, becuase it return a single int nad is not used*)
	| Eunop(Op_not,expr) ->let _ = codegen_arithmatic expr in (print_not (get_register_string (local_register_count)) (get_register_string (local_register_count));local_register_count) 
	| Ebracket(expr) -> codegen_arithmatic expr
	| Elval(lvalue) ->(incr cur_register_count;let temp_stack_num = get_lvalue_stack_num (!cur_func_symbol_hash_table) lvalue  in
		let temp_is_lvalue_ref = get_lvalue_ref_or_not (!cur_func_symbol_hash_table) lvalue in
			(if temp_is_lvalue_ref 
			then 
				(print_load (get_register_string local_register_count) temp_stack_num ;
					print_load_indirect (get_register_string local_register_count) (get_register_string local_register_count) ;
					local_register_count)
			else
				 (print_load (get_register_string local_register_count) temp_stack_num;
					local_register_count))) (* lvalue could be nested for a.x.c =>  *)
	| _ -> (Printf.printf "error at code gen ";exit 0)


	(*type checked before executing this line*)
let rec codegen_arithmatic_ref expr = let local_register_count = !cur_register_count in match expr with
	| Ebool(false) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) 0;
		local_register_count)
	| Ebool(true) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) 1;
		local_register_count)
	| Eint(int_val) -> (incr cur_register_count;
		print_int_const (get_register_string (local_register_count)) int_val;
		local_register_count)
	| Ebinop(expr_one,binop,expr_two) -> let left = codegen_arithmatic expr_one in
		let right = codegen_arithmatic expr_two in
			(decr cur_register_count; (*decrease register used*)
			codegen_binop (get_register_string local_register_count) (get_register_string left) (get_register_string right) binop;
			local_register_count)
	| Eunop(Op_minus,expr) -> (print_int_const (get_register_string local_register_count) (-1);
		print_mul_int (get_register_string local_register_count) (get_register_string local_register_count) (get_register_string(codegen_arithmatic expr));
		local_register_count) (*codegen_Arithmatic expr alone in here cause warnning, becuase it return a single int nad is not used*)
	| Eunop(Op_not,expr) ->let _ = codegen_arithmatic expr in (print_not (get_register_string (local_register_count)) (get_register_string (local_register_count));local_register_count) 
	| Ebracket(expr) -> codegen_arithmatic expr
	| Elval(lvalue) ->(incr cur_register_count;let temp_stack_num = get_lvalue_stack_num (!cur_func_symbol_hash_table) lvalue  in
		let temp_is_lvalue_ref = get_lvalue_ref_or_not (!cur_func_symbol_hash_table) lvalue in
			(if temp_is_lvalue_ref 
			then 
				(print_load (get_register_string local_register_count) temp_stack_num ;
					local_register_count)
			else
				 (print_load_address (get_register_string local_register_count) temp_stack_num;
					local_register_count))) (* lvalue could be nested for a.x.c =>  *)
	| _ -> (Printf.printf "error at code gen ";exit 0)

	(*| Eident(ident) -> (incr cur_register_count;
		try let temp_stack_num = Hashtbl.find cur_func_symbol_hash_table ident in
		print_load temp_stack_num (get_register_string local_register_count) with Not_found -> (Printf.printf "%s is not in the hash table " ident;exit 0)
		local_register_count)*)
(*
let codegen_stmts funcdef = match funcdef with
	| (funcDec,typeDef_list,stmt_list) -> (List.map (fun x -> match x with
		| Write expr -> codegen_arithmatic expr) stmt_list;
		Printf.printf "end\n") *)


(*dont know why following function does not work when called from other method*)
