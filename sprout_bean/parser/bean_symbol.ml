open Bean_ast

let top_level_expr_type = ref BeanTypeNone (*for case like write x > 4+1;, to know that i should print bool*)
let cur_expr_type = ref BeanTypeNone


let cur_register_count = ref (-1)
let cur_label_count = ref 0

let hash_table_size = 20

let stack_count = ref (-1)

(*cur method*)
let symbol_table_hash = Hashtbl.create hash_table_size

(*typedefs*)
let typdef_table_hash = Hashtbl.create hash_table_size

let func_stack_num_hash = Hashtbl.create hash_table_size (* local variables *)

let func_param_symbol_table_hash = Hashtbl.create hash_table_size (* params  *)

let cur_func_symbol_hash_table = ref (Hashtbl.create hash_table_size)

let func_param_order_hash_table = Hashtbl.create hash_table_size


(*record number of stack needed*)
let symbol_table_stackNum_hash = Hashtbl.create hash_table_size

let get_hash_table_typedef typedefTableType = match typedefTableType with
	| Typedef_Struct(hash_table) -> hash_table
	| _ -> (Printf.printf "get typedef hash failed "; exit 0)

let get_hash_table_symbol symbolTableType = match symbolTableType with
	|	S_Hash(_,hash_table) -> hash_table
	| S_Ref_Hash(_,hash_table) -> hash_table
	| S_Intext_Hash(hash_table) -> hash_table
  | S_Ref_Intext_Hash(hash_table) -> hash_table
  | S_Func(hash_table) -> hash_table
	| _ -> (Printf.printf "get symbol hash failed "; exit 0)



let rec build_one_typedef_table_hash_ hash_table typedefStruct_list = (List.iter (fun x -> 
	match x with
	| SingleTypeTermWithIdent(ident,SingleTypeTerm(bean_type)) -> (Hashtbl.add hash_table ident (Typedef_Struct_Sinlge_Type(bean_type)))
	| SingleTypeTermWithIdent(ident,ListTypeTerm(typedefStruct_list_inner)) ->(Hashtbl.add hash_table ident (Typedef_Struct(Hashtbl.create hash_table_size));
		build_one_typedef_table_hash_ (get_hash_table_typedef (Hashtbl.find hash_table ident)) typedefStruct_list_inner)
	| _ ->(Printf.printf "unexpect typedefStruct "; exit 0)
	) typedefStruct_list)

let build_one_typedef_table_hash one_typdef = match one_typdef with
	| (SingleTypeTerm(Bool),ident) -> Hashtbl.add typdef_table_hash ident (Typedef_Struct_Sinlge_Type(Bool))(*typedefs : (typedefStruct*ident) list;*)
	| (SingleTypeTerm(Int),ident) -> Hashtbl.add typdef_table_hash ident (Typedef_Struct_Sinlge_Type(Int))
	| (ListTypeTerm(typedefStruct_list),ident) -> (Hashtbl.add typdef_table_hash ident (Typedef_Struct(Hashtbl.create hash_table_size));
		build_one_typedef_table_hash_ (get_hash_table_typedef (Hashtbl.find typdef_table_hash ident)) typedefStruct_list)
	| (SingleTypeTerm(IdentType type_name),ident) ->(try Hashtbl.add typdef_table_hash ident (Hashtbl.find typdef_table_hash type_name) with Not_found -> (Printf.printf "Typedef order error ,adding type => %s ,finding type => %s\n" ident type_name; exit 0) )
	| _ ->(Printf.printf "build_one_typedef_table_hash error ";exit 0)

let build_typedef_table_hash typdefs = List.iter (build_one_typedef_table_hash) typdefs


let rec build_symbol_table_self_type hash_table typedef_hash_table = Hashtbl.iter (fun key value ->( 
	stack_count:= !stack_count + 1;
	match value with
		|Typedef_Struct_Sinlge_Type (Bool) ->Hashtbl.add hash_table key (S_Bool(Bool,!stack_count))
		|Typedef_Struct_Sinlge_Type (Int) ->Hashtbl.add hash_table key (S_Ref_Int(Int,!stack_count))
		|Typedef_Struct (typdef_hash_table) -> (
			stack_count:= !stack_count - 1;
			Hashtbl.add hash_table key (S_Hash(IdentType(key),Hashtbl.create hash_table_size));
			build_symbol_table_self_type (get_hash_table_symbol (Hashtbl.find hash_table key)) typdef_hash_table )
		| _ -> (Printf.printf "symbol table self type error \n" ; exit 0)
	)) typedef_hash_table
(*Hashtbl.add hash_table (key_prefix^"."^key) *)


(*key can be used to represent name*)
let rec build_symbol_table_typedefStruct_list  hashtable typedefStruct_list = List.iter (fun x -> (
	incr stack_count;
	match x with
		| SingleTypeTermWithIdent(var_name,SingleTypeTerm(Bool)) -> Hashtbl.add hashtable var_name (S_Bool(Bool,!stack_count));
		| SingleTypeTermWithIdent(var_name,SingleTypeTerm(Int)) -> Hashtbl.add hashtable var_name (S_Int(Int,!stack_count));
		| SingleTypeTermWithIdent(var_name,SingleTypeTerm(IdentType(type_name))) -> (let temp_type = Hashtbl.find typdef_table_hash type_name in
			match temp_type with
					|Typedef_Struct_Sinlge_Type (Bool) ->Hashtbl.add hashtable var_name (S_Bool(Bool,!stack_count)) (*dont need ref ?*)
					|Typedef_Struct_Sinlge_Type (Int) ->Hashtbl.add hashtable var_name (S_Int(Int,!stack_count))
					|Typedef_Struct (typdef_hash_table) -> (
						stack_count:= !stack_count - 1;(*dec stack num beacuse x.a , x => is actually not in the stack, only its filed is in the stack*)
						Hashtbl.add hashtable var_name (S_Hash(IdentType(type_name),Hashtbl.create hash_table_size));
						build_symbol_table_self_type (get_hash_table_symbol(Hashtbl.find hashtable var_name)) typdef_hash_table ) (* according to type name get the corresponding hash table *)
					|_ -> (Printf.printf "build symbol table failed ";exit 0))
		| SingleTypeTermWithIdent(var_name, ListTypeTerm(typedefStruct_list_inner)) ->(Hashtbl.add hashtable var_name (S_Intext_Hash(Hashtbl.create hash_table_size));(*if got one ref mean whole ref*)
			build_symbol_table_typedefStruct_list (get_hash_table_symbol(Hashtbl.find hashtable var_name)) typedefStruct_list_inner )
		| _ -> (Printf.printf "build symbol table failed outer";exit 0))) typedefStruct_list


let build_symbol_table_hash_funcDecParamList hash_table funcDecParamList = List.iter (fun x -> ( 
	incr stack_count;
	match x with
		| (Val , SingleTypeTerm(Bool) , param_name) -> Hashtbl.add hash_table param_name (S_Bool(Bool,!stack_count))
		| (Val , SingleTypeTerm(Int) , param_name) -> Hashtbl.add hash_table param_name (S_Int(Int,!stack_count))
		| (Val , SingleTypeTerm(IdentType(type_name)) , param_name) ->(let temp_type = Hashtbl.find typdef_table_hash type_name in
			match temp_type with
					|Typedef_Struct_Sinlge_Type (Bool) ->Hashtbl.add hash_table param_name (S_Bool(Bool,!stack_count))
					|Typedef_Struct_Sinlge_Type (Int) ->Hashtbl.add hash_table param_name (S_Int(Int,!stack_count))
					|Typedef_Struct (typdef_hash_table) -> (decr stack_count;(*dec stack num beacuse x.a , x => is actually not in the stack, only its filed is in the stack*)
						Hashtbl.add hash_table param_name (S_Hash(IdentType(type_name),Hashtbl.create hash_table_size));
						build_symbol_table_self_type (get_hash_table_symbol(Hashtbl.find hash_table param_name)) typdef_hash_table ) (* according to type name get the corresponding hash table *)
					|_ -> (Printf.printf "build symbol table failed \n";exit 0))
		| (Val , ListTypeTerm(typedefStruct_list) , param_name) -> (decr stack_count;Hashtbl.add hash_table param_name (S_Intext_Hash(Hashtbl.create hash_table_size));
			build_symbol_table_typedefStruct_list (get_hash_table_symbol(Hashtbl.find hash_table param_name)) typedefStruct_list)  (* {a:int } a *)
		| (Ref , SingleTypeTerm(Bool) , param_name) ->  Hashtbl.add hash_table param_name (S_Ref_Bool(Bool,!stack_count));
		| (Ref , SingleTypeTerm(Int) , param_name) ->  Hashtbl.add hash_table param_name (S_Ref_Int(Int,!stack_count));
		| (Ref , SingleTypeTerm(IdentType(type_name)) , param_name) ->(let temp_type = Hashtbl.find typdef_table_hash type_name in
			match temp_type with
					|Typedef_Struct_Sinlge_Type (Bool) ->Hashtbl.add hash_table param_name (S_Ref_Bool(Bool,!stack_count))
					|Typedef_Struct_Sinlge_Type (Int) ->Hashtbl.add hash_table param_name (S_Ref_Int(Int,!stack_count))
					|Typedef_Struct (typdef_hash_table) -> (decr stack_count;(*dec stack num beacuse x.a , x => is actually not in the stack, only its filed is in the stack*)
						Hashtbl.add hash_table param_name (S_Ref_Hash(IdentType(type_name),Hashtbl.create hash_table_size));
						build_symbol_table_self_type (get_hash_table_symbol(Hashtbl.find hash_table param_name)) typdef_hash_table )
					|_ -> (Printf.printf "build symbol table failed ";exit 0))
		|	(Ref , ListTypeTerm(typedefStruct_list) , param_name) -> (decr stack_count;Hashtbl.add hash_table param_name (S_Ref_Intext_Hash(Hashtbl.create hash_table_size));
			build_symbol_table_typedefStruct_list (get_hash_table_symbol(Hashtbl.find hash_table param_name)) typedefStruct_list )  (* {a:int } a *)
		| _ -> (Printf.printf "funcDecParam error. \n"; exit 0 ))) funcDecParamList




(*initial function name and its corresponding type is in type S_Hash*)

(*check duplicate function name in here ?*)
let build_symbol_table_hash_all funcDefs= List.iter (fun x ->(
	stack_count := -1;
	match x with
		|((func_name,funcDecParamList),typedefStruct_list,_) ->(Hashtbl.add symbol_table_hash func_name (S_Func((Hashtbl.create hash_table_size)));(*func name is stored*)
			(*Hashtbl.add func_param_symbol_table_hash func_name (S_Func((Hashtbl.create hash_table_size)));*)
			build_symbol_table_hash_funcDecParamList (get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name)) funcDecParamList;
			build_symbol_table_typedefStruct_list (get_hash_table_symbol(Hashtbl.find symbol_table_hash func_name)) typedefStruct_list;
			Hashtbl.add func_stack_num_hash func_name (!stack_count+1);
			Hashtbl.add func_param_order_hash_table func_name x (*param struct added *)
			))) funcDefs

let print_func_stack_num_hash func_stack_num_hash = (Printf.printf "----- Start Printing func_stack_num_hash -----\n" ;
		Hashtbl.iter (fun key value -> Printf.printf "Function name => %s, Function Max Stack Num => %d\n" key value ) func_stack_num_hash;
		Printf.printf "----- End Printing func_stack_num_hash -----\n" )

(*				| S_Struct(bean_type,stack_num)-> Printf.printf " type => ident , stack number => %d  \n" stack_num  (*struct name, *)
				| S_Ref_Struct(bean_type,stack_num)-> Printf.printf " Ref type => ident , stack number => %d  \n" stack_num  (*struct name, *)

			REAMOVED *)
let rec print_out_one_symbol_table one_symbol_table = (Printf.printf "----- Start Printing Sub Symbol Table -----\n";
	Hashtbl.iter (fun key value ->( 
		Printf.printf "Name  => %s ," key;
			match value with
				| S_Func(sub_symbol_table) -> (Printf.printf " Function Type => :";
						print_out_one_symbol_table sub_symbol_table)
				| S_Ref_Hash(IdentType(type_name),sub_symbol_table) ->(Printf.printf " Ref type => %s : \n" type_name;
						print_out_one_symbol_table sub_symbol_table)(*self def type*)
				| S_Hash(IdentType(type_name),sub_symbol_table) ->(Printf.printf " type => %s : \n" type_name;
						print_out_one_symbol_table sub_symbol_table)(*self def type*)
				| S_Bool(bean_type,stack_num) -> Printf.printf " type => Bool , stack number => %d  \n" stack_num (*Int => stack num*)
				| S_Int(bean_type,stack_num) -> Printf.printf " type => Int , stack number => %d  \n" stack_num 
				| S_Intext_Hash(sub_symbol_table)->(Printf.printf " Intext Type => :";
						print_out_one_symbol_table sub_symbol_table)
				| S_Ref_Int(bean_type,stack_num) -> Printf.printf " Ref type => Bool , stack number => %d  \n" stack_num 
				| S_Ref_Bool(bean_type,stack_num) -> Printf.printf " Ref type => Bool , stack number => %d  \n" stack_num 
				| S_Ref_Intext_Hash(sub_symbol_table)->(Printf.printf " Intext Type => :";
						print_out_one_symbol_table sub_symbol_table)
				| _ -> (Printf.printf " print_out_one_symbol_table error \n";exit 0)
	)) one_symbol_table;
	Printf.printf "----- End Printing Sub Symbol Table -----\n")


let rec print_out_one_typedef_table one_typedef_table = (Printf.printf "----- Start Printing Sub Typedef -----\n";
	Hashtbl.iter (fun key value ->
		match value with
			|Typedef_Struct_Sinlge_Type(bean_type) ->(match bean_type with
				| Bool -> Printf.printf " var => %s, type => Bool\n" key
				| Int -> Printf.printf " var => %s, type => Int\n" key
				| IdentType(ident) -> Printf.printf "Shouldn't have this \n"
				| _ -> (Printf.printf "print_out_one_typedef_table type error \n ";exit 0))
			|Typedef_Struct(one_sub_table) -> (Printf.printf " var => %s:" key;
				print_out_one_typedef_table one_sub_table)
			|Typedef_None -> Printf.printf "Error ! \n"
		) one_typedef_table;
	Printf.printf "----- End Printing Sub Typedef -----\n")




(* let build_symbol_table_hash *)