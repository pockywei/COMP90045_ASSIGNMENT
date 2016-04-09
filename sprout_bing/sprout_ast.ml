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

let printBeanType btype =  match btype with
| Bool -> Printf.printf "bean type : Bool " 
| Int -> Printf.printf "bean type : Int "
| IdentType(ident) -> Printf.printf "bean type : %s " ident


let rec printTypedefStruct typedefStructData = match typedefStructData with
| SingleTypeTerm (btype) -> printBeanType btype
| SingleTypeTermWithIdent (ident,nestTypedefStructData) -> (Printf.printf "ident : %s ," ident;
 printTypedefStruct nestTypedefStructData ; 
 Printf.printf "\n")
| ListTypeTerm (listTypedefStructData) ->(Printf.printf "nest: \n" ;
  List.iter (printTypedefStruct) listTypedefStructData ;
 Printf.printf "nest end \n")
| TypedefEnd -> Printf.printf "end with type def \n"

let printSingleTypedef singleTypedefData = match singleTypedefData with
| (typedefStruct,ident) -> (Printf.printf "Typedef Name : %s \n" ident ;
 printTypedefStruct typedefStruct;Printf.printf "\n")


let printTypedefList typedefDataList = (Printf.printf "Start typedef : \n" ;
 List.iter (printSingleTypedef) typedefDataList)

let printFuncIndicator funcIndicator = match funcIndicator with
| Val -> Printf.printf " func_indicator : Val "
| Ref -> Printf.printf " func_indicator : Ref "

let printFuncparams singleFuncparamData =  match singleFuncparamData with
| (funcIndicator,typedefStruct,ident) -> (Printf.printf "ident : %s " ident ;
  printFuncIndicator funcIndicator ; printTypedefStruct typedefStruct)

let printFuncheader funcheaderData = match funcheaderData with
| (funcname,funcparams) -> (Printf.printf "funcname : %s " funcname ;
List.iter (printFuncparams) funcparams)

let printFuncVardef funcVardefData = (Printf.printf "var dec start : \n";
  List.iter (printTypedefStruct) funcVardefData ;
  Printf.printf "var dec end : \n")

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






let printSingleFuncdef singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) -> ( printFuncheader funcheader;
  printFuncVardef funcvardef ; printFuncBody funcbody )

let printFuncdefList funcdefDataList = (Printf.printf "Start funcdef : \n" ; List.iter (printSingleFuncdef) funcdefDataList)






