(** The is the ast file contains our data type to store all the tokens
 * Program Description : This program is for the project of COMP90045 
 * at the University of Melbourne,
 * it is a compiler program for the bean language
 *
 * Team Member : 
 * Angus Huang 640386
 * Bingfeng Liu 639187
 * Chesdametrey Seng 748852
 * Chenhao Wei 803931
 *
 * Project Created Date : 18.03.2016
 *)

(* see the data type comments in bean_ast.mli*)

type ident = string
 
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

type valRef = 
|Val
|Ref

type funcDecParamList = (valRef*typedefStruct*string) list

type functionDeclaration = (string*funcDecParamList)

type program = {
  typedefs : (typedefStruct*ident) list;
  funcdefs : (functionDeclaration*typedefStruct list*stmt list) list
}
type t = program

let printBeanType fmt (btype,suffix) =  match btype with
| Bool -> Format.fprintf fmt "bool%s" suffix 
| Int ->  Format.fprintf fmt "int%s" suffix  
| IdentType(ident) -> Format.fprintf fmt "%s%s" ident suffix

let rec printTypedefStruct fmt (arrangeMode,typedefStructData) = match typedefStructData with
| SingleTypeTerm (btype) -> if arrangeMode = 1 then printBeanType fmt (btype,"") else printBeanType fmt (btype," ")
| SingleTypeTermWithIdent (ident,nestTypedefStructData) ->
  if arrangeMode = 1 then Format.fprintf fmt "%s : %a" ident printTypedefStruct (arrangeMode,nestTypedefStructData)
  else Format.fprintf fmt "%a%s" printTypedefStruct (arrangeMode,nestTypedefStructData) ident
| ListTypeTerm (listTypedefStructData) ->(Format.fprintf fmt "{"  ;
  List.iter (fun x -> if x = List.nth listTypedefStructData ((List.length listTypedefStructData)-1) && x == List.nth listTypedefStructData ((List.length listTypedefStructData)-1)
    then printTypedefStruct fmt (arrangeMode,x)
    else (printTypedefStruct fmt (arrangeMode,x) ; Format.fprintf fmt ", ")) listTypedefStructData;
  Format.fprintf fmt "}" )
| TypedefEnd -> Format.fprintf fmt "end with type def \n"

let printSingleTypedef fmt singleTypedefData = let arrangeMode = 1 in match singleTypedefData with
| (typedefStruct,ident) -> Format.fprintf fmt "typedef %a %s\n" printTypedefStruct (arrangeMode,typedefStruct) ident 
 

let printTypedefList fmt typedefDataList = (Format.fprintf fmt "@[";
  List.iter (printSingleTypedef fmt) typedefDataList;
  Format.fprintf fmt "@]@.")

let printFuncIndicator fmt funcIndicator = match funcIndicator with
| Val -> Format.fprintf fmt "val "
| Ref -> Format.fprintf fmt "ref "

let printFuncparams fmt singleFuncparamData =  let arrangeMode = 1 in match singleFuncparamData with
| (funcIndicator,typedefStruct,ident) -> Format.fprintf fmt  "%a%a %s"  printFuncIndicator funcIndicator printTypedefStruct (arrangeMode,typedefStruct) ident

let printFuncheader fmt funcheaderData = match funcheaderData with
| (funcname,funcparams) -> (Format.fprintf fmt "%s(" funcname ;
List.iter (fun x -> if x = List.nth funcparams ((List.length funcparams)-1) && x == List.nth funcparams ((List.length funcparams)-1)
    then printFuncparams fmt x
    else (printFuncparams fmt x ; Format.fprintf fmt ", ")) funcparams;
    Format.fprintf fmt ")")


let printFuncVardef fmt funcVardefData = let arrangeMode = 2 in List.iter (fun x -> 
   if x = List.nth funcVardefData ((List.length funcVardefData)-1) && x == List.nth funcVardefData ((List.length funcVardefData)-1)
   then (printTypedefStruct fmt (arrangeMode,x); Format.fprintf fmt ";")
   else (printTypedefStruct fmt (arrangeMode,x); Format.fprintf fmt ";@ "))
  funcVardefData


let rec printLvalue fmt singleLvalue = match singleLvalue with
| LId(ident) -> Format.fprintf fmt "%s" ident
| LField(recLvalue,ident) -> (printLvalue fmt recLvalue;Format.fprintf fmt ".";Format.fprintf fmt "%s" ident)
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

let rec loopToEndExpr expr = match expr with
| Ebracket(expr) -> loopToEndExpr expr
| _ -> expr


let highOrderBinoBefore binop expr =  match binop with
| Op_mul -> (match (loopToEndExpr expr) with 
  | Ebinop(expr_one,binop,expr_two) -> (match binop with
    | Op_mul -> false
    | Op_div -> false
    | _ -> true)
  | _ -> true)

| Op_div -> (match (loopToEndExpr expr) with 
  | Ebinop(expr_one,binop,expr_two) -> (match binop with
    | Op_mul -> false
    | Op_div -> false
    | _ -> true)
  | _ -> true)

| _ -> false


let highOrderBino binop = match binop with
| Op_mul -> true
| _ -> false

let nextBracket expr = match expr with
| Ebracket(expr) -> true
| _ ->false

(*false => do print, true => dont*)
let exprLookHead expr = match expr with 
| Ebool(bool_val) -> true
| Eint(int_val) -> true
| Elval(lvalue) -> true
| Ebinop(expr_one,binop,expr_two) ->if highOrderBino binop  then true else false
| Eunop(unop,expr) ->false
| Eident(ident) -> true
| Ebracket(expr) -> true

let printUnop fmt singleUnop = match singleUnop with
| Op_minus -> Format.fprintf fmt "-"
| Op_not -> Format.fprintf fmt " not "

let rec printExpr fmt (unNesBracket,singleExpr) = match singleExpr with
| Ebool(bool_val) -> Format.fprintf fmt "%B" bool_val
| Eint(int_val) -> Format.fprintf fmt "%d" int_val
| Elval(lvalue) -> printLvalue fmt lvalue
| Ebinop(expr_one,binop,expr_two) ->(printExpr fmt (highOrderBinoBefore binop expr_one,expr_one);
  printBinop fmt binop;
  printExpr fmt (true,expr_two))

| Eunop(unop,expr) -> (printUnop fmt unop ; printExpr fmt (true,expr))
| Eident(ident) -> Format.fprintf fmt "%s" ident
| Ebracket(expr) -> (if unNesBracket && not (exprLookHead expr)
  then Format.fprintf fmt "(%a)" printExpr (not (nextBracket expr),expr)
  else Format.fprintf fmt "%a" printExpr (true,expr))


let rec printRvalue fmt singleRvalue = match singleRvalue with
| Rexpr(expr) -> printExpr fmt (true,expr)
| RField(rvalue,expr) -> (printRvalue fmt rvalue;
  printExpr fmt (true,expr))
| Rassign(str,rvalue) -> (Format.fprintf fmt "%s = " str;
  printRvalue fmt rvalue)
| Rstmts(rvalueList) -> (Format.fprintf fmt "{";
  List.iter (fun x ->( if x = List.nth rvalueList ((List.length rvalueList)-1) && x == List.nth rvalueList ((List.length rvalueList)-1)
  then printRvalue fmt x
  else (printRvalue fmt x ;Format.fprintf fmt ", ") )) rvalueList;
  Format.fprintf fmt "}")
| Rempty -> Format.fprintf fmt "Empty Rvalue "

let getIdent identNum = String.make identNum ' '

let printEndStmt fmt isLast lastStr = match isLast with
  | true -> Format.fprintf fmt  "%s" lastStr
  | false -> Format.fprintf fmt  "%s@ " lastStr

let rec printStmt fmt (initIdent,isLast,singleStmt) = match singleStmt with

| Assign(lvalue, rvalue) -> (printLvalue fmt lvalue;
  Format.fprintf fmt  " := ";
  printRvalue fmt rvalue;
  printEndStmt fmt isLast ";")

| AssignRvalueList(lvalue,rvalueList) ->(printLvalue fmt lvalue;
  Format.fprintf fmt  "={";
  List.iter (printRvalue fmt) rvalueList;
  Format.fprintf fmt "}";
  printEndStmt fmt isLast ";")

| Read(lvalue) -> (Format.fprintf fmt  "read ";
  printLvalue fmt lvalue;
  printEndStmt fmt isLast ";")

| Write(expr) -> (Format.fprintf fmt  "write ";
  printExpr fmt (false,expr);
  printEndStmt fmt isLast ";")

| StmtNone -> Format.fprintf fmt "StmtNone"

| Method(methodname, paramList) -> (Format.fprintf fmt "%s(" methodname ;
  List.iter (fun x -> if x = List.nth paramList ((List.length paramList)-1) && x == List.nth paramList ((List.length paramList)-1) 
  then printExpr fmt (false,x)
  else (printExpr fmt (false,x); Format.fprintf fmt ", " )) paramList;
  Format.fprintf fmt ")";
  printEndStmt fmt isLast ";")

| VarDec(beantype, ident) -> (printBeanType fmt (beantype,"");
  Format.fprintf fmt "%s " ident;
  printEndStmt fmt isLast ";")

| WhileDec(expr, stmtList) ->(Format.fprintf fmt "while %a do @ " printExpr (false,expr);
  Format.fprintf fmt "@[<v %d>%s" initIdent (getIdent initIdent);
  List.iter (fun x -> if x = List.nth stmtList ((List.length stmtList)-1) && x == List.nth stmtList ((List.length stmtList)-1) 
    then printStmt fmt (initIdent,true,x)
    else printStmt fmt (initIdent,false,x)) stmtList;
  Format.fprintf fmt " @]";
  Format.fprintf fmt"@ od";
  printEndStmt fmt isLast "")

| IfDec(expr, thenStmtList, elseStmtList) -> (Format.fprintf fmt "if %a then @ " printExpr (false,expr);
  Format.fprintf fmt "@[<v %d>%s" initIdent (getIdent initIdent);
  List.iter (fun x -> if x = List.nth thenStmtList ((List.length thenStmtList)-1) && x == List.nth thenStmtList ((List.length thenStmtList)-1) 
    then printStmt fmt (initIdent,true,x)
    else printStmt fmt (initIdent,false,x)) thenStmtList;
  Format.fprintf fmt " @]";
  if (List.length elseStmtList) != 0 then (Format.fprintf fmt " @ ";Format.fprintf fmt "else @ ");
  Format.fprintf fmt "@[<v %d>%s" initIdent (getIdent initIdent);
  List.iter (fun x -> if x = List.nth elseStmtList ((List.length elseStmtList)-1) && x == List.nth elseStmtList ((List.length elseStmtList)-1)
    then printStmt fmt (initIdent,true,x)
    else printStmt fmt (initIdent,false,x)) elseStmtList;
  Format.fprintf fmt " @]";
  Format.fprintf fmt  "@ fi";
  printEndStmt fmt isLast "")

let printFuncBody fmt funcBodyData = let initIdentFactor = 4 in List.iter (fun x -> if x = List.nth funcBodyData ((List.length funcBodyData)-1) && x == List.nth funcBodyData ((List.length funcBodyData)-1)
    then printStmt fmt (initIdentFactor,true,x)
    else printStmt fmt (initIdentFactor,false,x)) funcBodyData

let printSingleFuncdef fmt singleFuncdefData = match singleFuncdefData with
| (funcheader,funcvardef,funcbody) ->(Format.fprintf fmt "proc %a" printFuncheader funcheader;
  if (List.length funcvardef) !=0 then Format.fprintf fmt " @ %a " printFuncVardef funcvardef;
  if (List.length funcbody) != 0 then Format.fprintf fmt " @ @ %a " printFuncBody funcbody;
  Format.fprintf fmt "\nend\n\n")

let printFuncdefList fmt funcdefDataList = let iniIdent = 4 in (Format.fprintf fmt "@[<v %d>" iniIdent ; List.iter (printSingleFuncdef fmt) funcdefDataList ;Format.fprintf fmt "@] " )









