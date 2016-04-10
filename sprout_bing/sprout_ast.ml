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

let printBeanType fmt btype =  match btype with
| Bool -> Format.fprintf fmt "bool" 
| Int ->  Format.fprintf fmt "int"  
| IdentType(ident) -> Format.fprintf fmt "%s" ident


let rec printTypedefStruct fmt typedefStructData = match typedefStructData with
| SingleTypeTerm (btype) -> printBeanType fmt btype
| SingleTypeTermWithIdent (ident,nestTypedefStructData) ->
  Format.fprintf fmt "%s : %a" ident printTypedefStruct nestTypedefStructData;
| ListTypeTerm (listTypedefStructData) ->(Format.fprintf fmt "{"  ;
  List.iter (fun x -> if x = List.nth listTypedefStructData ((List.length listTypedefStructData)-1) 
    then printTypedefStruct fmt x
    else (printTypedefStruct fmt x ; Format.fprintf fmt ", ")) listTypedefStructData;
  Format.fprintf fmt "}" )
| TypedefEnd -> Format.fprintf fmt "end with type def \n"

let printSingleTypedef fmt singleTypedefData = match singleTypedefData with
| (typedefStruct,ident) -> Format.fprintf fmt "@[typedef %a %s\n" printTypedefStruct typedefStruct ident 
 

let printTypedefList fmt typedefDataList = (Format.fprintf fmt "@[";
  List.iter (printSingleTypedef fmt) typedefDataList;
  Format.fprintf fmt "@]@.")




(* Print method delc*)


let printFuncIndicator fmt funcIndicator = match funcIndicator with
| Val -> Format.fprintf fmt "Val "
| Ref -> Format.fprintf fmt "Ref "

let printFuncparams fmt singleFuncparamData =  match singleFuncparamData with
| (funcIndicator,typedefStruct,ident) -> Format.fprintf fmt  "%a %a %s"  printFuncIndicator funcIndicator printTypedefStruct typedefStruct ident

let printFuncheader fmt funcheaderData = match funcheaderData with
| (funcname,funcparams) -> (Format.fprintf fmt "%s( " funcname ;
List.iter (fun x -> if x = List.nth funcparams ((List.length funcparams)-1) 
    then printFuncparams fmt x
    else (printFuncparams fmt x ; Format.fprintf fmt ", ")) funcparams;
    Format.fprintf fmt ")")


let printFuncVardef fmt funcVardefData =List.iter (fun x -> 
   if x = List.nth funcVardefData ((List.length funcVardefData)-1)
   then (printTypedefStruct fmt x; Format.fprintf fmt ";")
   else (printTypedefStruct fmt x; Format.fprintf fmt ";@,"))
  funcVardefData


(*//TODO *)
let rec printLvalue fmt singleLvalue = match singleLvalue with
| LId(ident) -> Format.fprintf fmt "%s" ident
| LField(recLvalue,ident) -> (Format.fprintf fmt "%s." ident; printLvalue fmt recLvalue)
| LvalueNone -> Format.fprintf fmt "Empty lvalue"

let printBinop fmt singleBinop = match singleBinop with
| Op_add -> Format.fprintf fmt  " + "
| Op_sub -> Format.fprintf fmt  " - "
| Op_mul -> Format.fprintf fmt  " * "
| Op_div -> Format.fprintf fmt " / " 
| Op_eq -> Format.fprintf fmt  " = "
| Op_lt -> Format.fprintf fmt  " < "
| Op_gt -> Format.fprintf fmt  " > "
| Op_neq -> Format.fprintf fmt  " != "
| Op_lte -> Format.fprintf fmt  " <= "
| Op_gte -> Format.fprintf fmt  " >= "
| Op_and -> Format.fprintf fmt  " and "
| Op_or -> Format.fprintf fmt  " or "

let printUnop fmt singleUnop = match singleUnop with
| Op_minus -> Format.fprintf fmt "-"
| Op_not -> Format.fprintf fmt "!"

let rec printExpr fmt singleExpr = match singleExpr with
| Ebool(bool_val) -> Format.fprintf fmt "%B " bool_val
| Eint(int_val) -> Format.fprintf fmt "%d " int_val
| Elval(lvalue) -> printLvalue fmt lvalue
| Ebinop(expr_one,binop,expr_two) -> (printExpr fmt expr_one;
  printBinop fmt binop; printExpr fmt expr_two)
| Eunop(unop,expr) -> (printUnop fmt unop ; printExpr fmt expr)
| Eident(ident) -> Format.fprintf fmt "%s " ident
| Ebracket(expr) -> (Format.fprintf fmt "( " ; printExpr fmt expr  ;Format.fprintf fmt " ) ")


let rec printRvalue fmt singleRvalue = match singleRvalue with
| Rexpr(expr) -> printExpr fmt expr
| RField(rvalue,expr) -> (printRvalue fmt rvalue;
  printExpr fmt expr)
| Rassign(str,rvalue) -> (Format.fprintf fmt "%s " str;
  printRvalue fmt rvalue)
| Rstmts(rvalueList) -> List.iter (printRvalue fmt) rvalueList
| Rempty -> Format.fprintf fmt "Empty Rvalue "

let getIdent identNum = String.make identNum ' '

let rec printStmt fmt (initIdent,isLast,singleStmt) = match singleStmt with

| Assign(lvalue, rvalue) -> (printLvalue fmt lvalue;
  Format.fprintf fmt  " = ";
  printRvalue fmt rvalue;
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| AssignRvalueList(lvalue,rvalueList) ->(printLvalue fmt lvalue;
  Format.fprintf fmt  "={";
  List.iter (printRvalue fmt) rvalueList;
  Format.fprintf fmt "}";
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| Read(lvalue) -> (Format.fprintf fmt  "Read ";
  printLvalue fmt lvalue;
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| Write(expr) -> (Format.fprintf fmt  "Write ";
  printExpr fmt expr;
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| StmtNone -> Format.fprintf fmt "StmtNone"

| Method(methodname, paramList) -> (Format.fprintf fmt "%s( " methodname ;
  List.iter (fun x -> if x = List.nth paramList ((List.length paramList)-1) 
  then printExpr fmt x
  else (printExpr fmt x; Format.fprintf fmt ", " )) paramList;
  Format.fprintf fmt " )";
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| VarDec(beantype, ident) -> (Format.fprintf fmt "%s " ident;
  printBeanType fmt beantype;
  if isLast = true
  then Format.fprintf fmt  ";"
  else Format.fprintf fmt  ";@ ")

| WhileDec(expr, stmtList) ->(Format.fprintf fmt "While %a do @ " printExpr expr;
  Format.fprintf fmt "@[<v 4>%s" (getIdent 4);
  List.iter (fun x -> if x = List.nth stmtList ((List.length stmtList)-1) 
    then printStmt fmt (initIdent+1,true,x)
    else printStmt fmt (initIdent+1,false,x)) stmtList;
  Format.fprintf fmt " @]";
  Format.fprintf fmt"@ od";
  if isLast = true
  then Format.fprintf fmt  " "
  else Format.fprintf fmt  "@ ")

| IfDec(expr, thenStmtList, elseStmtList) -> (Format.fprintf fmt "If %a then @ " printExpr expr;
  Format.fprintf fmt "@[<v 4>%s" (getIdent 4);
  List.iter (fun x -> if x = List.nth thenStmtList ((List.length thenStmtList)-1) 
    then printStmt fmt (initIdent+1,true,x)
    else printStmt fmt (initIdent+1,false,x)) thenStmtList;
  Format.fprintf fmt " @]";
  Format.fprintf fmt " @ ";
  Format.fprintf fmt "eles @ ";
  Format.fprintf fmt "@[<v 4>%s" (getIdent 4);
  List.iter (fun x -> if x = List.nth elseStmtList ((List.length elseStmtList)-1) 
    then printStmt fmt (initIdent+1,true,x)
    else printStmt fmt (initIdent+1,false,x)) elseStmtList;
  Format.fprintf fmt " @]";
  Format.fprintf fmt  "@ fi";
  if isLast = true
  then Format.fprintf fmt  " "
  else Format.fprintf fmt  "@ ")

let printFuncBody fmt funcBodyData = let initIdentFactor = 1 in List.iter (fun x -> if x = List.nth funcBodyData ((List.length funcBodyData)-1) 
    then printStmt fmt (initIdentFactor,true,x)
    else printStmt fmt (initIdentFactor,false,x)) funcBodyData
(*
let printSingleFuncdef fmt initIdent initIdentFactor singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) -> Format.fprintf fmt "@[proc %a @ %a @ @ %a end@]@." printFuncheader (initIdent,initIdentFactor,funcheader) printFuncVardef (initIdent,initIdentFactor,funcvardef)  printFuncBody (initIdent,initIdentFactor,funcbody)
*)
let printSingleFuncdef fmt singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) -> Format.fprintf fmt "proc %a @ @ %a @ @ %a \nend\n\n" printFuncheader funcheader printFuncVardef funcvardef printFuncBody funcbody

let printFuncdefList fmt funcdefDataList = let iniIdent = 4 in (Format.fprintf fmt "@[<v %d>" iniIdent ; List.iter (printSingleFuncdef fmt) funcdefDataList ;Format.fprintf fmt "@] " )









