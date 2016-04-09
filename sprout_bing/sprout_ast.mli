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
val printBeantype : beantype -> unit 
val printTypedefStruct : typedefStruct -> unit 
val printTypedefs : (typedefStruct list * ident) list -> unit
*)
(*
val printBeanType : beantype -> unit 
val printSingleTypeDef : typedefStruct * ident -> unit
val printTypedefList : (typedefStruct * ident) list -> unit 
*)
(*
val printBeanType : beantype -> unit 
val printTypedefStruct : typedefStruct -> unit
val printSingleTypedef : typedefStruct * ident -> unit
val printTypedefList : (typedefStruct * ident) list -> unit
*)
(*
val printFuncIndicator : valRef -> unit
val printFuncparams : valRef * typedefStruct * ident -> unit
val printFuncheader : ident * (valRef * typedefStruct * ident) list -> unit
val printFuncVardef : typedefStruct list -> unit
val printLvalue : lvalue -> unit
val printBinop : binop -> unit
val printUnop : unop -> unit
val printExpr : expr -> unit
val printRvalue : rvalue -> unit
val printStmt : stmt -> unit
val printFuncBody : stmt list -> unit
val printSingleFuncdef : (ident * (valRef * typedefStruct * ident) list) * typedefStruct list * stmt list -> unit 
val printFuncdefList : ((ident * (valRef * typedefStruct * ident) list) * typedefStruct list * stmt list) list -> unit*)


val printBeanType : Format.formatter -> int * int * beantype -> unit
val printTypedefStruct : Format.formatter -> int * int * typedefStruct -> unit
val printSingleTypedef : Format.formatter -> int -> int -> typedefStruct * ident -> unit
val printTypedefList : Format.formatter -> (typedefStruct * ident) list -> unit
