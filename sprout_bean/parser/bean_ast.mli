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


(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | IdentType of string 
  | BeanTypeNone

(* store type and var declaration*)
type typedefStruct =
  |SingleTypeTerm of beantype
  |SingleTypeTermWithIdent of (ident * typedefStruct)
  |ListTypeTerm of typedefStruct list
  |TypedefEnd
type typedef = typedefStruct list

(* lvalue type *)
type lvalue =
  | LId of ident
  | LField of (lvalue * ident) (* recursively lvalue e.g. var.var1.var2*)
  | LvalueNone

(* binop represent binary operator e.g. +, *, etc *)
type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_lt | Op_gt | Op_neq | Op_lte | Op_gte
  | Op_and | Op_or

(* unop => unary operator *)
type unop =
  | Op_minus | Op_not

(* expression type *)
type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Eident of string
  | Ebracket of expr

(* store different form of rvalue *)
type rvalue =
  | Rexpr of expr
  | RField of (rvalue * expr)
  | Rassign of (string * rvalue)
  | Rstmts of rvalue list 
  | Rempty

(* represent the data return from yacc for lis of arguments *)
type paramList = expr list

(* represent different method procedures  *)
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

(* represent parameter indicator either ref or val *)
type valRef = 
|Val
|Ref


type stackNum = int

type typedefTableType = 
  |Typedef_Struct_Sinlge_Type of beantype
  |Typedef_Struct of ((string , typedefTableType) Hashtbl.t)
  |Typedef_None

type symbolTableType =
  | S_Func of (string , symbolTableType) Hashtbl.t
  | S_Ref_Hash of (beantype * (string , symbolTableType) Hashtbl.t)(* stored nest type of typedef*)
  | S_Hash of (beantype * (string, symbolTableType) Hashtbl.t)(*self def type*)
  | S_Bool of (beantype * stackNum) (*Int => stack num*)
  | S_Int of (beantype * stackNum)
  | S_Ref_Int of (beantype * stackNum)
  | S_Ref_Bool of (beantype * stackNum)(*if beantype is a ident, need to search through typedef hash table*)
  | S_Intext_Hash of (string , symbolTableType) Hashtbl.t
  | S_Ref_Intext_Hash of (string , symbolTableType) Hashtbl.t

(* represent the parameters declaration for method header *)
type funcDecParamList = (valRef*typedefStruct*string) list

(* represent function name and its parameters *)
type functionDeclaration = (string*funcDecParamList)

(* the whole bean language tokens is stored in here*)
type program = {
  typedefs : (typedefStruct*ident) list;
  funcdefs : (functionDeclaration*typedefStruct list*stmt list) list
}

(* type used by pretty print *)
type t = program

(* foresee  what is before this current expr, to decide printing parenthesis*)
val highOrderBinoBefore : binop -> expr -> bool
(* print bean type *)
val printBeanType : Format.formatter -> beantype * ident -> unit
(* print corresponding data in typedefStruct *)
val printTypedefStruct : Format.formatter -> int * typedefStruct -> unit
(* print typedef data *)
val printSingleTypedef : Format.formatter -> typedefStruct * ident -> unit
(* print typedef list *)
val printTypedefList : Format.formatter -> (typedefStruct * ident) list -> unit
(* print function indicator *)
val printFuncIndicator : Format.formatter -> valRef -> unit
(* print function parameters with its indicator and type *)
val printFuncparams : Format.formatter -> valRef * typedefStruct * ident -> unit
(* print function header *)
val printFuncheader : Format.formatter -> ident * (valRef * typedefStruct * ident) list -> unit
(* print function variable declaration  *)
val printFuncVardef : Format.formatter -> typedefStruct list -> unit
(* print lvalue *)
val printLvalue : Format.formatter -> lvalue -> unit
(* print binary operator *)
val printBinop : Format.formatter -> binop -> unit
(* print unary operator *)
val printUnop : Format.formatter -> unop -> unit
(* print expression *)
val printExpr : Format.formatter -> bool * expr -> unit
(* print rvalue *)
val printRvalue : Format.formatter -> rvalue -> unit
(* fill spaces to avoid nested vertical boxes newline issue *)
val getIdent : int -> ident
(* print statements end without @ *)
val printEndStmt : Format.formatter -> bool -> ident -> unit
(* print statements *)
val printStmt : Format.formatter -> int * bool * stmt -> unit
(* print function body *)
val printFuncBody : Format.formatter -> stmt list -> unit
(* print function definition *)
val printSingleFuncdef : Format.formatter -> (ident * (valRef * typedefStruct * ident) list) * typedefStruct list * stmt list -> unit
(* print list of function definitions *)
val printFuncdefList : Format.formatter -> ((ident * (valRef * typedefStruct * ident) list) * typedefStruct list * stmt list) list -> unit
(* to find next expr apart from Ebracket *)
val loopToEndExpr : expr -> expr
(* to see if next Ebracket *)
val nextBracket : expr -> bool