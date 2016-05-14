open Bean_ast
open Bean_symbol
(* *)
(* == , this compare refernece, = compare content equality*)
let t_flag = ref true
let t0_free = ref true
let cur_register_count = ref 0
let cur_label_count = ref 0

let print_push_stack_frame frame_size = Printf.printf "push_stack_frame %d\n" frame_size

let print_pop_stack_frame frame_size = Printf.printf "pop_stack_frame %d\n" frame_size

let print_load register_name slot_num = Printf.printf "load %s, %d" register_name slot_num

let print_sotre slot_num register_name = Printf.printf "store %d, %s" slot_num register_name

let print_load_address register_name slot_num = Printf.printf "load_address %s, %d" register_name slot_num

let print_load_indirect register_name_1 register_name_2 = Printf.printf "load_indirect %s, %s" register_name_1 register_name_2

let print_store_indirect register_name_1 register_name_2 = Printf.printf "store_indirect %s, %s" register_name_1 register_name_2

let print_int_const register_name int_const_value = Printf.printf "int_const %s, %d" register_name int_const_value

let print_string_const register_name string_const_value = Printf.printf "string_const %s, %s" register_name string_const_value

let print_add_int register_1 register_2 register_3 = Printf.printf "add_int %s, %s, %s" register_1 register_2 register_3

let print_add_offset register_1 register_2 register_3 = Printf.printf "add_offset %s, %s, %s" register_1 register_2 register_3

let print_sub_int register_1 register_2 register_3 = Printf.printf "sub_int %s, %s, %s" register_1 register_2 register_3

let print_sub_offset register_1 register_2 register_3 = Printf.printf "sub_offset %s, %s, %s" register_1 register_2 register_3

let print_mul_int register_1 register_2 register_3 = Printf.printf "mul_int %s, %s, %s" register_1 register_2 register_3

let print_div_int register_1 register_2 register_3 = Printf.printf "div_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_eq_int register_1 register_2 register_3 = Printf.printf "cmp_eq_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_ne_int register_1 register_2 register_3 = Printf.printf "cmp_ne_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_gt_int register_1 register_2 register_3 = Printf.printf "cmp_gt_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_ge_int register_1 register_2 register_3 = Printf.printf "cmp_ge_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_lt_int register_1 register_2 register_3 = Printf.printf "cmp_lt_int %s, %s, %s" register_1 register_2 register_3

let print_cmp_le_int register_1 register_2 register_3 = Printf.printf "cmp_le_int %s, %s, %s" register_1 register_2 register_3

let print_and register_1 register_2 register_3 = Printf.printf "and %s, %s, %s" register_1 register_2 register_3

let print_or register_1 register_2 register_3 = Printf.printf "or %s, %s, %s" register_1 register_2 register_3

let print_not register_1 register_2 = Printf.printf "not %s, %s" register_1 register_2

let print_move register_1 register_2 = Printf.printf "move %s, %s" register_1 register_2

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


let printBinop singleBinop = match singleBinop with
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
let rec codegen_arithmatic expr = let local_register_count = !cur_register_count in match expr with
	| Ebool(bool_val) -> (cur_register_count := !cur_register_count+1;
		Printf.printf "Load T%d %B\n" local_register_count bool_val;
		local_register_count)
	| Eint(int_val) -> (cur_register_count := !cur_register_count+1;
		Printf.printf "Load T%d %d\n" local_register_count int_val;
		local_register_count)
	| Ebinop(expr_one,binop,expr_two) -> let left = codegen_arithmatic expr_one in
		let right = codegen_arithmatic expr_two in
			( cur_register_count := !cur_register_count - 1;
			Printf.printf "T%d = T%d %s T%d \n" local_register_count left (printBinop binop) right;
			local_register_count)
	| Eident(ident) -> (cur_register_count := !cur_register_count+1;
		Printf.printf "Load T%d %s\n" local_register_count ident;
		local_register_count)
	| Ebracket(expr) -> codegen_arithmatic expr


let codegen_stmts funcdef = match funcdef with
	| (funcDec,typeDef_list,stmt_list) -> (List.map (fun x -> match x with
		| Write expr -> codegen_arithmatic expr) stmt_list;
		Printf.printf "end\n")


(*dont know why following function does not work when called from other method*)
let inc_cur_register_count() = cur_register_count := !cur_register_count + 1
