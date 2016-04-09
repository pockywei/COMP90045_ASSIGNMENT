(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | IdentType of string 

type typedefStruct =
  |SingleTypeTerm of beantype
  |SingleTypeTermWithIdent of (ident * typedefStruct)
  |ListTypeTerm of typedefStruct list
  |TypedefEnd
type typedef = typedefStruct list

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)
  | LvalueNone

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_lt | Op_gt | Op_neq | Op_lte | Op_gte
  | Op_and | Op_or

type unop =
  | Op_minus | Op_not

type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Eident of string
  | Ebracket of expr

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr
  | RField of (rvalue * expr)
  | Rassign of (string * rvalue)
  | Rstmts of rvalue list 
  | Rempty


type paramList = expr list

type stmt = 
  | Assign of (lvalue * rvalue)
  | AssignRvalueList of (lvalue * rvalue list)
  | Read of lvalue
  | Write of expr
  | StmtNone
  | Method of (string * paramList)
  | VarDec of (beantype * string)
  | WhileDec of (expr * stmt list)
  | IfDec of (expr * stmt list * stmt list)




type decl = (ident * beantype)
(*
type stmt =  
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Test
*)

type valRef = 
|Val
|Ref


type funcDecParamList = (valRef*typedefStruct*string) list


type functionDeclaration = (string*funcDecParamList)
(*
type program = {
  typedefs : (typedef*ident) list;
  funcdefs : (functionDeclaration*stmst list*stmt list) list
}
*)
type program = {
  typedefs : (typedefStruct*ident) list;
  funcdefs : (functionDeclaration*typedefStruct list*stmt list) list
}
type t = program

(*
let printBeantype beanTypeData = match beanTypeData with
| Bool -> Printf.printf " Bool "
| Int -> Printf.printf " Int "
| IdentType(s) ->  Printf.printf " string => %s" s

let rec printTypedefStruct typedefStructData = match typedefStructData with
| SingleTypeTerm(id , tp) -> (Printf.printf "id => %s , type => \n" id ; printBeantype tp)
| ListTypeTerm(id , tps) -> (Printf.printf "Listype name => %s " id;List.iter (printTypedefStruct) tps)

let rec printTypedefs typedefData = match typedefData with
| [] -> Printf.printf "Empty \n "
| (hstructList,hid)::t -> ( Printf.printf "typedef name => %s : \n  " hid; List.iter (printTypedefStruct) hstructList ; printTypedefs t )

*)

(* Print typedef *)

let printBeanType fmt (initIdent,initIdentFactor,btype) =  match btype with
| Bool -> Format.fprintf fmt "@[<v %d>bool@]" (initIdent*initIdentFactor) 
| Int ->  Format.fprintf fmt "@[<v %d>int@]" (initIdent*initIdentFactor) 
| IdentType(ident) -> Format.fprintf fmt "@[<v %d>%s@]" (initIdent*initIdentFactor) ident


let rec printTypedefStruct fmt (initIdent,initIdentFactor,typedefStructData) = match typedefStructData with
| SingleTypeTerm (btype) -> printBeanType fmt (initIdent,initIdentFactor,btype)
| SingleTypeTermWithIdent (ident,nestTypedefStructData) ->
  Format.fprintf fmt "@[<v %d>%s : %a @]" (initIdent*initIdentFactor) ident printTypedefStruct (initIdent,initIdentFactor,nestTypedefStructData);
| ListTypeTerm (listTypedefStructData) ->(Format.fprintf fmt "@[<v %d>{@]" (initIdent*initIdentFactor) ;
  List.iter (fun x -> if x = List.nth listTypedefStructData ((List.length listTypedefStructData)-1) 
    then printTypedefStruct fmt (initIdent,initIdentFactor,x)
    else (printTypedefStruct fmt (initIdent,initIdentFactor,x) ; Format.fprintf fmt ", ")) listTypedefStructData;
  Format.fprintf fmt "@[<v %d>}@]" (initIdent*initIdentFactor))
| TypedefEnd -> Format.fprintf fmt "end with type def \n"

let printSingleTypedef fmt initIdent initIdentFactor singleTypedefData = match singleTypedefData with
| (typedefStruct,ident) -> Format.fprintf fmt "@[typedef %a %s@]@." printTypedefStruct (initIdent,initIdentFactor,typedefStruct) ident 
 

let printTypedefList fmt typedefDataList = let initIdent = 0
in let initIdentFactor = 1 in 
List.iter (printSingleTypedef fmt initIdent initIdentFactor) typedefDataList




(* Print method delc*)


let printFuncIndicator fmt (initIdent,initIdentFactor,funcIndicator) = match funcIndicator with
| Val -> Format.fprintf fmt "Val "
| Ref -> Format.fprintf fmt "Ref "

let printFuncparams fmt (initIdent,initIdentFactor,singleFuncparamData) =  match singleFuncparamData with
| (funcIndicator,typedefStruct,ident) -> Format.fprintf fmt  "@[%a %a %s@]"  printFuncIndicator (initIdent,initIdentFactor,funcIndicator) printTypedefStruct (initIdent,initIdentFactor,typedefStruct) ident

let printFuncheader fmt (initIdent,initIdentFactor,funcheaderData) = match funcheaderData with
| (funcname,funcparams) -> (Format.fprintf fmt "@[%s(@] " funcname ;
List.iter (fun x -> if x = List.nth funcparams ((List.length funcparams)-1) 
    then printFuncparams fmt (initIdent,initIdentFactor,x)
    else (printFuncparams fmt (initIdent,initIdentFactor,x) ; Format.fprintf fmt ", ")) funcparams;
    Format.fprintf fmt "@[)@]@. ")


let printFuncVardef fmt (initIdent,initIdentFactor,funcVardefData) = List.iter (fun x -> (printTypedefStruct fmt (initIdent,initIdentFactor,x); Format.fprintf fmt "@[;@]@.")) funcVardefData
(*
let rec printLvalue singleLvalue = match singleLvalue with
| LId(ident) -> Printf.printf " %s " ident
| LField(recLvalue,ident) -> (Printf.printf "%s." ident; printLvalue recLvalue)
| LvalueNone -> Printf.printf "Empty lvalue "

let printBinop singleBinop = match singleBinop with
| Op_add -> Printf.printf " + "
| Op_sub -> Printf.printf " - "
| Op_mul -> Printf.printf " * "
| Op_div -> Printf.printf " / " 
| Op_eq -> Printf.printf " = "
| Op_lt -> Printf.printf " < "
| Op_gt -> Printf.printf " > "
| Op_neq -> Printf.printf " != "
| Op_lte -> Printf.printf " <= "
| Op_gte -> Printf.printf " >= "
| Op_and -> Printf.printf " and "
| Op_or -> Printf.printf " or "

let printUnop singleUnop = match singleUnop with
| Op_minus -> Printf.printf "-"
| Op_not -> Printf.printf "!"

let rec printExpr singleExpr = match singleExpr with
| Ebool(bool_val) -> Printf.printf " %B " bool_val
| Eint(int_val) -> Printf.printf " %d " int_val
| Elval(lvalue) -> printLvalue lvalue
| Ebinop(expr_one,binop,expr_two) -> (printExpr expr_one;
  printBinop binop; printExpr expr_two)
| Eunop(unop,expr) -> (printUnop unop ; printExpr expr)
| Eident(ident) -> Printf.printf "%s " ident
| Ebracket(expr) -> (Printf.printf " ( " ; printExpr expr  ;Printf.printf " ) ")


let rec printRvalue singleRvalue = match singleRvalue with
| Rexpr(expr) -> printExpr expr
| RField(rvalue,expr) -> (printRvalue rvalue;
  printExpr expr)
| Rassign(str,rvalue) -> (Printf.printf "%s " str;
  printRvalue rvalue)
| Rstmts(rvalueList) -> List.iter (printRvalue) rvalueList
| Rempty -> Printf.printf "Empty Rvalue "


let rec printStmt singleStmt = match singleStmt with
| Assign(lvalue, rvalue) -> (printLvalue lvalue;
  Printf.printf "=";
  printRvalue rvalue;
  Printf.printf " \n")
| AssignRvalueList(lvalue,rvalueList) ->(printLvalue lvalue;
  Printf.printf "={";
  List.iter (printRvalue) rvalueList;
  Printf.printf "}";
  Printf.printf " \n")
| Read(lvalue) -> (Printf.printf "Read ";
  printLvalue lvalue;
  Printf.printf " \n")
| Write(expr) -> (Printf.printf "Write ";
  printExpr expr;
  Printf.printf " \n")
| StmtNone -> (Printf.printf "StmtNone";
  Printf.printf " \n")
| Method(methodname, paramList) -> (Printf.printf "%s( " methodname ;
  List.iter (printExpr) paramList;
  Printf.printf " ) \n")
| VarDec(beantype, ident) -> (Printf.printf "%s " ident;
  printBeanType beantype;
  Printf.printf " \n")
| WhileDec(expr, stmtList) ->(Printf.printf "While do ";
  printExpr expr;
  List.iter (printStmt) stmtList;
  Printf.printf "od ";)
| IfDec(expr, thenStmtList, elseStmtList) -> (Printf.printf "If ";
  printExpr expr;
  List.iter (printStmt) thenStmtList;
  List.iter (printStmt) elseStmtList;
  Printf.printf "fi")

let printFuncBody funcBodyData = (Printf.printf "start func body : \n";
  List.iter (printStmt) funcBodyData;
  Printf.printf "end func body \n")
*)
(*
let printSingleFuncdef fmt initIdent initIdentFactor singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) -> Format.fprintf fmt "@[proc %a @ %a @ @ %a end@]@." printFuncheader (initIdent,initIdentFactor,funcheader) printFuncVardef (initIdent,initIdentFactor,funcvardef)  printFuncBody (initIdent,initIdentFactor,funcbody)
*)
let printSingleFuncdef fmt initIdent initIdentFactor singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) -> Format.fprintf fmt "@[proc %a @ %a @ @ %a @ end @]@." printFuncheader (initIdent,initIdentFactor,funcheader) printFuncVardef (initIdent,initIdentFactor+1,funcvardef)

let printFuncdefList fmt funcdefDataList =  let initIdent = 4
in let initIdentFactor = 0 in List.iter (printSingleFuncdef fmt initIdent initIdentFactor) funcdefDataList
