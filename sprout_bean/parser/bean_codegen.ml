open Bean_ast
(* *)
(* == , this compare refernece, = compare content equality*)
let t_flag = ref true
let t0_free = ref true
let cur_register_count = ref 0
let cur_label_count = ref 0

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
