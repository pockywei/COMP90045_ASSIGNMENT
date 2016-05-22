open Bean_ast
open Bean_symbol
open Bean_codegen

let register_count = ref (-1)
let cur_param_ref = ref false



let rec get_cur_LId lvalue = match lvalue with
	| LId(ident) -> ident
  | LField(lvalue,ident) -> ident
  | _ -> (raise (Failure "Error at get_cur_LId"))

let get_rest_lvalue lvalue= match lvalue with
  | LField(lvalue,ident) -> lvalue
  | _ -> (raise (Failure "get_rest_lvalue"))



let get_symbol_hash_table_primitive_type hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(bean_type , _) -> bean_type
  | S_Int(bean_type , _) -> bean_type
  (*| S_Struct(bean_type , _) -> bean_type*)
  | S_Ref_Int(bean_type , _) -> bean_type
  | S_Ref_Bool(bean_type , _) -> bean_type (*only return Bool or Int, typedef of {} type will cause error*)
  | S_Hash(bean_type,_) -> bean_type
  | S_Ref_Hash(bean_type,_)->bean_type
  | _ -> (raise (Failure "get primitive type error\n"))

let get_symbol_hash_table_primitive_type_stack_num symbol_type = match symbol_type with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  (*| S_Struct(bean_type , _) -> bean_type*)
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ ,stackNum) -> stackNum (*only return Bool or Int, typedef of {} type will cause error*)
  | _ -> (raise (Failure "get primitive type error\n"))



let expr_type_match_with_cur_expr_type bean_type = (if ((!cur_expr_type) = BeanTypeNone) then cur_expr_type := bean_type;
	if (bean_type = (!cur_expr_type)) then true else false)

let rec get_lvalue_type hash_table lvalue = match lvalue with
	| LId(ident) -> get_symbol_hash_table_primitive_type hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	(get_lvalue_type temp_hash_symbol_table lvalue_type)
  | _ -> (raise (Failure"error on checking lvalue type \n"))




(*for write*)
let rec check_expr_type hash_table expr = match expr with
	| Ebool(bool_val) -> expr_type_match_with_cur_expr_type Bool
  | Eint(int_val) -> expr_type_match_with_cur_expr_type Int
  | Elval(lvalue) -> expr_type_match_with_cur_expr_type (get_lvalue_type hash_table lvalue)
  | Ebinop(expr_1,binop,expr_2) ->(let _ = check_expr_type hash_table expr_1 in check_expr_type hash_table expr_2)
  | Eunop(unop,expr_1) -> check_expr_type hash_table expr_1
  | Eident(ident) -> expr_type_match_with_cur_expr_type (IdentType(ident)) (* "string " *)
  (*| Eident(ident) -> try let temp_type_result = Hashtbl.find hash_table ident in
  	 	expr_type_match_with_cur_expr_type (get_symbol_hash_table_primitive_type(temp_type_result))
  	 with Not_found -> (Printf.printf "checking Eident error when checking expr type\n";exit 0)*)   (*assume ident is in frist level , not with dot*)
  | Ebracket(expr) ->check_expr_type hash_table expr

let rec getStackNum hash_table key_name = match (Hashtbl.find hash_table key_name) with
  | S_Bool(_ , stackNum) -> stackNum
  | S_Int(_ , stackNum) -> stackNum
  (*| S_Struct(_ , stackNum) -> stackNum*)
  | S_Ref_Int(_ , stackNum) -> stackNum
  | S_Ref_Bool(_ , stackNum) -> stackNum
  | _ -> (raise (Failure "get stack num error\n"))



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
  | _ -> (raise (Failure "type error for is ref \n"))

(*true => ref, false => val *)
let get_bool_ref_val_symbol_type symbol_type = match symbol_type with
  | S_Bool(_ , _) -> false
  | S_Int(_ , _) -> false
  | S_Hash(_,_) -> false
  | S_Intext_Hash(_) -> false
  | S_Ref_Hash(_,_) -> true
  | S_Ref_Int(_ , _) -> true
  | S_Ref_Bool(_ , _) -> true (*only return Bool or Int, typedef of {} type will cause error*)
  | S_Ref_Intext_Hash(_) -> true

  (* add intext S_Intext_Hash of (string , symbolTableType) Hashtbl.t , S_Ref_Intext_Hash of (string , symbolTableType) Hashtbl.t *)
  | _ -> (Printf.printf "check val ref type error\n"; exit 0)

let rec get_lvalue_ref_or_not hash_table key_name = match key_name with
	| LId(ident) -> get_bool_ref_val_symbol_hash_table hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_ref_or_not temp_hash_symbol_table lvalue_type
  | _ -> (raise (Failure "error on checking lvalue type \n"))




let rec get_lvalue_symbol_type hash_table lvalue = match lvalue with
	| LId(ident) -> Hashtbl.find hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_symbol_type temp_hash_symbol_table lvalue_type
  | _ -> (raise (Failure "error on checking lvalue type \n"))

let rec add_in_Lid_to_Last l_field add_on_string = match l_field with 
  | LId(ident) -> LField(LId(add_on_string),ident)
  | LField(lvalue_type,ident) -> LField( (add_in_Lid_to_Last lvalue_type add_on_string),ident)
  | _ -> (raise (Failure "error add_in_Lid_to_Last \n"))

(*
 let rec get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum(Hashtbl.find hash_table ident)
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table = Hashtbl.find hash_table ident in 
  	get_lvalue_stack_num temp_hash_symbol_table lvalue_type
  | _ -> (Printf.printf"error on get stack num lvalue type \n"; exit 0)
*)


let rec parse_print_store symbol_struct = match symbol_struct with
  | S_Bool(bean_type , stackNum) -> print_store (stackNum) (get_register_string (!cur_register_count))
  | S_Int(bean_type , stackNum) -> print_store (stackNum) (get_register_string (!cur_register_count))
  | S_Hash(bean_type,inner_hash_table) -> Hashtbl.iter (fun key value -> parse_print_store value) inner_hash_table
  | S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> parse_print_store value) inner_hash_table
  | S_Ref_Hash(bean_type,inner_hash_table) -> Hashtbl.iter (fun key value -> parse_print_store value) inner_hash_table
  | S_Ref_Int(bean_type , stackNum) -> print_store (stackNum) (get_register_string (!cur_register_count))
  | S_Ref_Bool(bean_type , stackNum) -> print_store (stackNum) (get_register_string (!cur_register_count))
  | S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> parse_print_store value) inner_hash_table
  | _ -> (Printf.printf "error parse_print_store \n";exit 0)

let rec get_lvalue_stack_num hash_table lvalue = match lvalue with
	| LId(ident) -> getStackNum hash_table ident
  | LField(lvalue_type,ident) -> let temp_hash_symbol_table =  get_hash_table_symbol (Hashtbl.find hash_table ident) in 
  	get_lvalue_stack_num temp_hash_symbol_table lvalue_type
  | _ -> (raise (Failure "error on get stack num lvalue type \n"))

(*
let rec do_print_rassign is_ref lvalue hash_table expr = match (get_lvalue_symbol_type lvalue) with
  | S_Ref_Hash(inner_hash_table) -> (Hashtbl.iter (fun key value -> 
    ) inner_hash_table )
  | S_Hash(inner_hash_table)
  | S_Bool(bean_type,stackNum)
  | S_Int(bean_type,stackNum)
  | S_Ref_Int(bean_type,stackNum)
  | S_Ref_Bool(bean_type,stackNum)
  | S_Intext_Hash(inner_hash_table)
  | S_Ref_Intext_Hash(inner_hash_table)
  | -> (Printf.printf "error do_print_rassign\n";exit 0)
*)

(*must be lvalue*)
(*
let codegen_load_lvalue_typedef is_ref hash_table symbol_struct = match symbol_struct with
    | S_Ref_Hash(inner_hash_table) -> (Hashtbl.iter (fun key value -> codegen_expr_typedef is_ref inner_hash_table value ) inner_hash_table )
    | S_Hash(inner_hash_table) -> (Hashtbl.iter (fun key value -> codegen_expr_typedef is_ref inner_hash_table value ) inner_hash_table )
    | S_Intext_Hash(inner_hash_table) ->(Hashtbl.iter (fun key value -> codegen_expr_typedef is_ref inner_hash_table value ) inner_hash_table )
    | S_Ref_Intext_Hash(inner_hash_table) ->(Hashtbl.iter (fun key value -> codegen_expr_typedef is_ref inner_hash_table value ) inner_hash_table )
    | S_Bool(bean_type,stackNum) -> codegen_arithmatic expr
    | S_Int(bean_type,stackNum) -> codegen_arithmatic expr
    | S_Ref_Int(bean_type,stackNum) -> codegen_arithmatic expr
    | S_Ref_Bool(bean_type,stackNum) -> codegen_arithmatic expr
    | -> (Printf.printf "error do_print_rassign\n";exit 0)


  | _ -> (Printf.printf "type mis match from codegen_expr_typedef\n";exit 0) 
*)

let rec codegen_store_rvalue lvalue_symbol_struct expr_symbol_struct is_ref_expr = match lvalue_symbol_struct with
  | S_Ref_Hash(bean_type,inner_hash_table) -> (Hashtbl.iter (fun key value -> 
    (let expr_value = Hashtbl.find (get_hash_table_symbol expr_symbol_struct) key in codegen_store_rvalue value expr_value is_ref_expr ) ) inner_hash_table )
  | S_Hash(bean_type,inner_hash_table) ->  (Hashtbl.iter (fun key value -> 
    (let expr_value = Hashtbl.find (get_hash_table_symbol expr_symbol_struct) key in codegen_store_rvalue value expr_value is_ref_expr ) ) inner_hash_table )
  | S_Intext_Hash(inner_hash_table) ->  (Hashtbl.iter (fun key value -> 
    (let expr_value = Hashtbl.find (get_hash_table_symbol expr_symbol_struct) key in codegen_store_rvalue value expr_value is_ref_expr ) ) inner_hash_table )
  | S_Ref_Intext_Hash(inner_hash_table) -> (Hashtbl.iter (fun key value -> 
    (let expr_value = Hashtbl.find (get_hash_table_symbol expr_symbol_struct) key in codegen_store_rvalue value expr_value is_ref_expr ) ) inner_hash_table )
  | S_Bool(bean_type,stackNum) ->( match is_ref_expr with
    | true -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" expr_stack_num ;
        print_load_indirect "r0" "r0" ;
        print_store stackNum "r0" )
    | false -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" expr_stack_num ;
        print_store stackNum "r0" ))
  | S_Int(bean_type,stackNum) ->( match is_ref_expr with
    | true -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" expr_stack_num ;(* right is pointer *)
        print_load_indirect "r0" "r0" ;
        print_store stackNum "r0" )
    | false -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" expr_stack_num ; (*right is not pointer , both not pointer*)
        print_store stackNum "r0" ))
  | S_Ref_Int(bean_type,stackNum) -> (match is_ref_expr with
    | true -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" stackNum ;
        print_load "r1" expr_stack_num;
        print_load_indirect "r1" "r1" ;(*both pointer*)
        print_store_indirect "r0" "r1" )
    | false -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" stackNum;(*left is poiner *)
        print_load "r1" expr_stack_num ;
        print_store_indirect "r0" "r1" ))
  | S_Ref_Bool(bean_type,stackNum) -> (match is_ref_expr with
    | true -> let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" stackNum ;
        print_load "r1" expr_stack_num;
        print_load_indirect "r1" "r1" ;(*both pointer*)
        print_store_indirect "r0" "r1" )
    | false ->let expr_stack_num = get_symbol_hash_table_primitive_type_stack_num expr_symbol_struct in 
      (print_load "r0" stackNum;(*left is poiner *)
        print_load "r1" expr_stack_num ;
        print_store_indirect "r0" "r1" ))
  | _ -> (raise (Failure "error do_print_rassign\n"))



(*key should be found in the hashtable other wise it is a type error *)
let rec process_rvalue is_ref lvalue hash_table rvalue = match rvalue with(*{a:int}, {b:int} not the same *)
  | Rexpr( Elval(lvalue_inner) ) ->(codegen_store_rvalue (get_lvalue_symbol_type (!cur_func_symbol_hash_table) lvalue) (get_lvalue_symbol_type (!cur_func_symbol_hash_table) lvalue_inner ) (get_lvalue_ref_or_not (!cur_func_symbol_hash_table)  lvalue_inner))
  | Rexpr(expr) -> let temp_var_stack_num = (get_lvalue_stack_num hash_table lvalue) in
     (cur_func_symbol_hash_table := hash_table ;
      cur_register_count := 0 ;
      let result_register = codegen_arithmatic expr in (if is_ref
        then (print_load (get_register_string (result_register+1)) temp_var_stack_num; (*assign new value to ref var*)
          print_store_indirect (get_register_string (result_register+1)) (get_register_string result_register)) (*r1 = r0 *)
        else print_store temp_var_stack_num (get_register_string result_register) )) (*answer is in r0 *)
  
  | Rassign (inner_var, inner_rvalue) -> let new_lvalue = add_in_Lid_to_Last lvalue inner_var in
        (process_rvalue is_ref new_lvalue hash_table inner_rvalue)
  (*| Rassign (inner_var, Rexpr(expr)) -> let new_lvalue = add_in_Lid_to_Last lvalue inner_var in
      let temp_var_stack_num = (get_lvalue_stack_num hash_table new_lvalue) in
        (process_rvalue is_ref new_lvalue hash_table (Rexpr(expr));(* a := {b=12+x,c=321}*)
          if is_ref(*load address to register*)
          then 
            (print_load "r1" temp_var_stack_num; (*assign new value to ref var*)
            print_store_indirect "r1" "r0") (*r1 = r0 *)
          else print_store 0 "r0")
  | Rassign (inner_var,Rstmts (rvalue_list)) -> process_rvalue is_ref (add_in_Lid_to_Last inner_var)  (Rstmts (rvalue_list))*)
  | Rstmts (rvalue_list) -> List.iter (process_rvalue is_ref lvalue hash_table ) rvalue_list (* a := {a=123,b=321}, if can't find in the hasb table mean error*)
  (* a := {b= ? ,c= ?}*)
  | _ -> (raise (Failure "rvalue processing error \n"))


let rec codgen_all_param_fields param_symbol_structure hash_table = let local_register_count = !cur_register_count in match param_symbol_structure with
  | S_Ref_Hash (bean_type,inner_hash_table) ->( Hashtbl.iter (fun kye value -> codgen_all_param_fields value inner_hash_table) inner_hash_table)
  | S_Hash (bean_type,inner_hash_table) -> Hashtbl.iter (fun kye value -> codgen_all_param_fields value inner_hash_table) inner_hash_table
  | S_Bool (bean_type, stackNum) -> (incr cur_register_count;print_store (stackNum) (get_register_string (local_register_count)))
  | S_Int (bean_type,stackNum) -> (incr cur_register_count;print_store (stackNum) (get_register_string (local_register_count)))
  | S_Ref_Int (bean_type,stackNum) -> (incr cur_register_count;print_store (stackNum) (get_register_string (local_register_count)))
  | S_Ref_Bool (bean_type ,stackNum) -> (incr cur_register_count;print_store (stackNum) (get_register_string (local_register_count)))
  | S_Intext_Hash (inner_hash_table) -> Hashtbl.iter (fun kye value -> codgen_all_param_fields value inner_hash_table) inner_hash_table
  | S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun kye value -> codgen_all_param_fields value inner_hash_table) inner_hash_table
  | _ -> (raise (Failure "Error on codgen_all_param_fields\n"))

let rec codegen_var_init hash_table one_struct = match one_struct with
	| SingleTypeTermWithIdent(var_name,ListTypeTerm(typedefStruct_list)) -> (try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table var_name) in
			List.iter (codegen_var_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (raise (Failure "codegen_var_init finding symbol table failed at ListTypeTerm\n")))
	| SingleTypeTermWithIdent(var_name,_) -> parse_print_store (Hashtbl.find hash_table var_name) 
	| _ -> (raise (Failure "Error on initializing local var codegen_var_init\n"))

let rec codegen_var_init_incr_ver hash_table one_struct =(match one_struct with
	| SingleTypeTermWithIdent(var_name,ListTypeTerm(typedefStruct_list)) -> (try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table var_name) in
			List.iter (codegen_var_init temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (raise (Failure "codegen_var_init finding symbol table failed at ListTypeTerm\n")))
	| SingleTypeTermWithIdent(var_name,_) -> parse_print_store (Hashtbl.find hash_table var_name)
	| _ -> (raise (Failure "Error on initializing local var codegen_var_init\n")))

let rec codegen_param_init hash_table one_param = match one_param with
  | (Val , ListTypeTerm(typedefStruct_list) , param_name) ->(try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table param_name) in
      List.iter (codegen_var_init_incr_ver temp_hash_symbol_table) typedefStruct_list with
    Not_found -> (raise (Failure "find hash failed codegen_param_init\n")))
	| (Ref , ListTypeTerm(typedefStruct_list) , param_name) ->(try let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find hash_table param_name) in
			List.iter (codegen_var_init_incr_ver temp_hash_symbol_table) typedefStruct_list with
		Not_found -> (raise (Failure "find hash failed codegen_param_init\n")))
	| (_ , _ , param_name) -> codgen_all_param_fields (Hashtbl.find (!cur_func_symbol_hash_table) param_name) (!cur_func_symbol_hash_table)


  
	(*| _ -> (Printf.printf "Code Gend Error in codegen_param_init \n";exit 0)*)
(*
let rec tranverse_hash_table_get_data hash_table stack_num = Hashtbl.iter (fun _ value-> match value with
	| S_Ref_Hash of (bean_type,inner_hash_table)
  | S_Hash of (bean_type,inner_hash_table)(*self def type*)
  | S_Bool of (bean_type * stackNum) (*Int => stack num*)
  | S_Int of (bean_type * stackNum)
  | S_Ref_Int of (bean_type * stackNum)
  | S_Ref_Bool of (bean_type * stackNum)(*if beantype is a ident, need to search through typedef hash table*)
  | S_Intext_Hash (inner_hash_table)
  | S_Ref_Intext_Hash of (string , symbolTableType) Hashtbl.t
	| (Printf.printf "Error on tranverse hash table with stack num\n";exit 0)) hash_table
*)


let check_ref_val_type_equal first second = match first with 
  | S_Ref_Hash (bean_type,inner_hash_table) -> (match second with
  	|S_Hash (bean_type_1,inner_hash_table_1) -> if bean_type = bean_type_1
  		then true
  		else false 
  	|S_Ref_Hash (bean_type_1,inner_hash_table_1) -> if bean_type = bean_type_1
  		then true
  		else false 
  	| _ -> false)
  | S_Hash (bean_type,inner_hash_table) -> (match second with
  	|S_Hash (bean_type_1,inner_hash_table_1) -> if bean_type = bean_type_1
  		then true
  		else false 
  	|S_Ref_Hash (bean_type_1,inner_hash_table_1) -> if bean_type = bean_type_1
  		then true
  		else false 
  	| _ -> false)
  | S_Bool (_,_) -> (match second with
  	| S_Bool (_,_) -> true
  	| S_Ref_Bool (_,_) -> true
  	| _ -> false)
  | S_Int (_,_) -> (match second with
  	| S_Int (_,_) -> true
  	| S_Ref_Int (_,_) -> true
  	| _ -> false)
  | S_Ref_Int (_,_) ->  (match second with
  	| S_Int (_,_) -> true
  	| S_Ref_Int (_,_) -> true
  	| _ -> false)
  | S_Ref_Bool (_,_) -> (match second with
  	| S_Bool (_,_) -> true
  	| S_Ref_Bool (_,_) -> true
  	| _ -> false)
  | S_Intext_Hash (inner_hash_table) -> (match second with
  	| S_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| S_Ref_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | S_Ref_Intext_Hash (inner_hash_table) ->( match second with
  	| S_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| S_Ref_Intext_Hash (inner_hash_table_1) -> if inner_hash_table = inner_hash_table_1
  		then true
  		else false 
  	| _ -> false)
  | _ -> (raise (Failure  "error check_ref_val_type_equal unwanted type\n" ))

let rec convert_one_expr_param_to_symbol_type one_expr = match one_expr with 
  | Ebool(_) -> S_Bool(Bool,-1) (*-1 means nothing ,just need this struct to compare with clee*)
  | Eint(_) -> S_Int(Int,-1) 
  | Elval(lvalue) -> get_lvalue_symbol_type (!cur_func_symbol_hash_table) lvalue
  | Ebinop (expr_1,binop,expr_2) -> (let _ = convert_one_expr_param_to_symbol_type expr_1 in convert_one_expr_param_to_symbol_type expr_2)
  | Eunop (unop,expr) -> convert_one_expr_param_to_symbol_type expr
  | Ebracket(expr)-> convert_one_expr_param_to_symbol_type expr
  | _ -> (raise (Failure "convert_one_expr_param_to_symbol_type type error\n"))
  (*| Eident(string_val)*) (*this is for " awefawef  " => type *)

let rec gen_symbol_type_to_register symbol_type is_ref_caller is_ref_callee   =
	let local_register_count = !cur_register_count in (match (is_ref_caller,is_ref_callee) with
	| (true,false) -> (match symbol_type with 		
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string local_register_count) (get_register_string local_register_count) (* dereference r *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string local_register_count) (get_register_string local_register_count) (* dereference r *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string local_register_count) (get_register_string local_register_count) (* dereference r *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s , s is addr *)
			print_load_indirect (get_register_string local_register_count) (get_register_string local_register_count) (* dereference r *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (raise (Failure "error on gen_symbol_type_to_register\n")))
	| (false,true) -> (match symbol_type with 		(* caller not ref, callee ref*)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string local_register_count) stack_num; (* r = &s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string local_register_count) stack_num; (* r = &s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string local_register_count) stack_num; (* r = &s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load_address (get_register_string local_register_count) stack_num; (* r = &s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (raise (Failure "error on gen_symbol_type_to_register\n")))
	| (true,true) -> (match symbol_type with (* caller ref, callee ref *)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (raise (Failure "error on gen_symbol_type_to_register\n")))
	| (false,false) -> (match symbol_type with (*caller val, callee val*)
		| S_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Hash(_,inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| S_Ref_Int(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *))
		| S_Ref_Bool(_ , stack_num) -> (incr cur_register_count;
			print_load (get_register_string local_register_count) stack_num; (* r = s *)) (*only return Bool or Int, typedef of {} type will cause error*)
		| S_Ref_Intext_Hash(inner_hash_table) -> Hashtbl.iter (fun key value -> gen_symbol_type_to_register value  is_ref_caller is_ref_callee ) inner_hash_table 
		| _ -> (raise (Failure "error on gen_symbol_type_to_register\n"))))


(*one_expr_param is from caller, one_param is from callee*)
let process_calling_method_param caller_hash_table callee_hash_table one_expr_param one_param =(match one_param with
	| (Val,_,one_callee_param_name) -> (cur_expr_type := BeanTypeNone;
		(*make sure has same type*)
		if (check_expr_type caller_hash_table one_expr_param)&&(check_ref_val_type_equal (Hashtbl.find callee_hash_table one_callee_param_name) (convert_one_expr_param_to_symbol_type one_expr_param))
		then match one_expr_param with
			| Ebool(bool_val) -> (codegen_arithmatic one_expr_param)
		  | Eint(int_val) -> (codegen_arithmatic one_expr_param)
		  | Elval(lvalue) -> if !cur_expr_type = Int || !cur_expr_type = Bool 
		  	then  codegen_arithmatic one_expr_param
		  	else  (Hashtbl.iter (fun key value -> gen_symbol_type_to_register value (get_lvalue_ref_or_not caller_hash_table lvalue) false ) (get_hash_table_symbol (get_lvalue_symbol_type caller_hash_table lvalue));
		  		!cur_register_count) (*must return a num...*)
		  | Ebinop (expr_1,binop,expr_2) -> (codegen_arithmatic one_expr_param)(* codege_ari incr cur_register inside *)
		  | Eunop (unop,expr) -> (codegen_arithmatic expr)
		  | Ebracket(expr)-> (codegen_arithmatic expr)
		  | _ -> (raise (Failure "process_calling_method_param error => then match one_expr_param with\n"))
		else (raise (Failure "caller and callee param type mismatch\n")))
		(*callee param is val*)
	| (Ref,_,one_callee_param_name) ->  (cur_expr_type := BeanTypeNone;
		(*make sure has same type*)
		if (check_expr_type caller_hash_table one_expr_param) && (check_ref_val_type_equal (Hashtbl.find callee_hash_table one_callee_param_name) (convert_one_expr_param_to_symbol_type one_expr_param))
		then match one_expr_param with
		(* ref does not allow primitive value pass in 
			| Ebool(bool_var) -> (codegen_arithmatic one_expr_param)
		  | Eint(int_val) -> (codegen_arithmatic one_expr_param ) *)
		  | Elval(lvalue) -> if !cur_expr_type = Int || !cur_expr_type = Bool 
		  	then  codegen_arithmatic_ref one_expr_param
		  	else  (Hashtbl.iter (fun key value -> gen_symbol_type_to_register value (get_lvalue_ref_or_not caller_hash_table lvalue) true ) (get_hash_table_symbol (get_lvalue_symbol_type caller_hash_table lvalue));
		  		!cur_register_count) (* passed in is a struct / typedef *)
		  (*| Ebinop (expr_1,binop,expr_2) -> (convert_one_expr_param_to_symbol_type expr_1; convert_one_expr_param_to_symbol_type expr_2) same reason primitive value cant be ref
		  | Eunop (unop,expr) -> convert_one_expr_param_to_symbol_type expr
		  | Ebracket(expr)-> convert_one_expr_param_to_symbol_type expr*)
		  | _ -> (raise (Failure "process_calling_method_param error => then match one_expr_param with\n"))
		else (raise (Failure"caller and callee param type mismatch\n"))))(*callee param is  ref *)
	(*| _ -> (Printf.printf "process_calling_method_param error";exit 0)*)

let rec codegen_one_stmt hash_table one_stmt =(cur_func_symbol_hash_table := hash_table; match one_stmt with
	| Assign(lvalue, rvalue) -> (cur_register_count := 0;process_rvalue (get_lvalue_ref_or_not hash_table lvalue) lvalue hash_table rvalue) (*set cur_expr_type to lvalue type, then in process_rvalue will check type*)
  | Read(lvalue) ->let temp_lvalue_type = get_lvalue_type hash_table lvalue in 
  	let temp_lvalue_stack_num = (get_lvalue_stack_num hash_table lvalue) in (*ref ?*)
	  	if(temp_lvalue_type = Int)
	  	then 
	  		if (get_lvalue_ref_or_not hash_table lvalue)(* is ref *)
	  		then (print_read_int();
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_int();
	  					print_store temp_lvalue_stack_num "r0")
	  	else 
	  		if (get_lvalue_ref_or_not hash_table lvalue)(* is ref *)
	  		then (print_read_bool(); (*result is in r0 *)
	  					print_load "r1" temp_lvalue_stack_num;
	  					print_store_indirect "r1" "r0" )
	  		else (print_read_bool();
	  					print_store temp_lvalue_stack_num "r0")
  | Write(expr) ->(top_level_expr_type:= BeanTypeNone;cur_expr_type := BeanTypeNone; cur_register_count := 0;
    check_expr_type hash_table expr;
    if true
  	then 
  		if(!cur_expr_type = Int) (*write int, result of arithematic is in r0*)
  		then 
  			(let _ =codegen_arithmatic expr in
  				(if !top_level_expr_type = Bool then print_print_bool () else print_print_int()))
  		else 
  			  match !cur_expr_type with (*write string*)
  				| IdentType(string_temp) -> (print_string_const (get_register_string 0) string_temp;
  					print_print_string())
          | Bool -> (let _ =codegen_arithmatic expr in
          print_print_bool())
  				| _ -> (Printf.printf "type error when writing a string\n";exit 0)
		else (raise (Failure "write check type failed \n")))
  | Method(method_name, expr_params_list) ->(cur_register_count := 0;
  	let temp_hash_symbol_table = get_hash_table_symbol (Hashtbl.find symbol_table_hash method_name) in (*callee hashtable*)
  		let ((callee_func_name,temp_param_list),_,_)  = Hashtbl.find func_param_order_hash_table method_name in(*(valRef*typedefStruct*string) list*) 
		  	(if (List.length expr_params_list) = (List.length temp_param_list)
		  	then (List.iter2 (fun first second -> (let _ = process_calling_method_param hash_table temp_hash_symbol_table first second in Printf.printf "")) expr_params_list temp_param_list )
		  	else (raise (Failure "short for params for calling %s\n")));
      print_call method_name;)
  | WhileDec(expr, stmt_list) -> (let while_label = (!cur_label_count) in 
	  	let while_out_label = (!cur_label_count) + 1 in 
		  	(print_label_by_number while_label;
          cur_label_count := (!cur_label_count) + 2;
		  		let result_register = codegen_arithmatic expr in (*compare condition *)
			  	(print_branch_on_false (get_register_string result_register) (get_label_name while_out_label); (*branch is different to call .*)
			  	List.iter (codegen_one_stmt hash_table) stmt_list; (**)
			  	print_branch_on_unc (get_label_name while_label);
			  	print_label_by_number while_out_label)))
	(*will reserve else label as well, eventhough it might have no else part*)
  | IfDec(expr,then_stmt_list,else_stmt_list) ->(let if_else_label = (!cur_label_count) in
  	let if_out_label= (!cur_label_count)+1 in ( cur_label_count:=(!cur_label_count)+2;
  		if(List.length (else_stmt_list))!= 0 (*if have else and condition is flase then go to else*)
  		then (cur_register_count := 0;
  			let result_register = codegen_arithmatic expr in
  			(print_branch_on_false (get_register_string result_register) (get_label_name if_else_label);
	  			List.iter (codegen_one_stmt hash_table) then_stmt_list ;
	  			print_branch_on_unc (get_label_name if_out_label);
	  			print_label_by_number if_else_label;
	  			List.iter (codegen_one_stmt hash_table) else_stmt_list;(*if false go to else label, if true say in and jump uncondition to out label*)
	  			print_label_by_number if_out_label))
  		else (cur_register_count := 0;
        let result_register = codegen_arithmatic expr in 
          (print_branch_on_false (get_register_string result_register) (get_label_name if_out_label);(*if not else and false go to out label*)
  			   List.iter (codegen_one_stmt hash_table) then_stmt_list;
  			   print_label_by_number if_out_label))))(* if not else directly go *) 
  | _ -> (raise (Failure "start_translate_by_function_stmt_list error \n") ))

(*

let check_if_ref func_name key = (*see is var ref or not*)

let check_one_stmt_type stmt_data type_data = (*write, read, assign, *)
*)


let start_translate_by_function_declaration func_name one_functionDeclaration = match one_functionDeclaration with
	|(func_name,funcDecParamList) -> (cur_register_count := 0;
		print_label_by_function_name func_name; (*start each label body*)
		(try let function_symbol_table_hash = get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name) in
			(print_push_stack_frame (Hashtbl.find func_stack_num_hash func_name);
				List.iter (fun x -> (
					codegen_param_init function_symbol_table_hash x)) funcDecParamList;
				cur_register_count := 0)
		with Not_found -> (raise (Failure "%s symbol_table_hash not found\n"))))

let rec start_translate_by_function_variable_declaration func_name typedefStruct_list = let var_init_symbol_table = get_hash_table_symbol( Hashtbl.find symbol_table_hash func_name )in
	(cur_register_count := 0;
		print_int_const (get_register_string !cur_register_count) 0; (*initialize everything to 0*)
		List.iter (codegen_var_init var_init_symbol_table)  typedefStruct_list)

let start_translate_by_function_stmt_list func_name stmt_list = let temp_hash_symbol_table = get_hash_table_symbol ( Hashtbl.find symbol_table_hash func_name) in
	( cur_expr_type := BeanTypeNone;
		cur_func_symbol_hash_table := temp_hash_symbol_table;
		List.iter (codegen_one_stmt temp_hash_symbol_table) stmt_list)


let start_translate_by_function one_funcdef = match one_funcdef with
	|((func_name,func_param_list),typedefStruct_list,stmt_list) ->(cur_func_symbol_hash_table := get_hash_table_symbol( Hashtbl.find symbol_table_hash func_name );
    start_translate_by_function_declaration func_name (func_name,func_param_list);
		start_translate_by_function_variable_declaration func_name typedefStruct_list;
		start_translate_by_function_stmt_list func_name stmt_list;
    print_pop_stack_frame (Hashtbl.find func_stack_num_hash func_name); (*pop stack at the end of function body*)
		print_return ())

(*syntax ?*)
let rec find_funcdef funcdefs func_name = try match (List.hd funcdefs) with
		| ((fun_name,_),_,_) -> if fun_name = func_name then List.hd funcdefs  else find_funcdef (List.tl funcdefs) func_name
	with Failure e-> (raise (Failure "no main function\n")) 

let start_test_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs);
		(*build_func_method_param_order_hash_table (prog.funcdefs);*)

		Printf.printf "----- Start Printing typdef_table_hash -----\n";
		print_out_one_typedef_table typdef_table_hash;
		Printf.printf "----- End Printing typdef_table_hash -----\n";

		Printf.printf "----- Start Printing symbol_table_hash -----\n";
		print_out_one_symbol_table symbol_table_hash;
		Printf.printf "----- End Printing symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_param_symbol_table_hash -----\n";
		(*print_out_one_symbol_table func_param_symbol_table_hash;*)
		Printf.printf "----- End Printing func_param_symbol_table_hash -----\n";

		Printf.printf "----- Start Printing func_stack_num_hash -----\n";
		print_func_stack_num_hash func_stack_num_hash;
		Printf.printf "----- End Printing func_stack_num_hash -----\n";

		Printf.printf "----- Start Generating Oz Code -----\n";
		print_call "main";
		print_halt ();
(*!= compare address ...*)
		let main_def = find_funcdef (prog.funcdefs) ("main") in
			(start_translate_by_function main_def;

		  List.iter (fun x ->( match x with 
        |((func_name,_),_,_) -> if not (func_name = "main") then start_translate_by_function x  )) prog.funcdefs))

let start_analyzer prog = (build_typedef_table_hash (prog.typedefs);
		build_symbol_table_hash_all (prog.funcdefs))
		(*print_out_one_typedef_table typdef_table_hash;*)
		(*print_out_one_symbol_table symbol_table_hash);*)
		(*translate main first *)

