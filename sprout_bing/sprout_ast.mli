(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | IdentType of string 

type typedefStruct =
  |SingleTypeTerm of (ident * beantype)
  |ListTypeTerm of (ident * typedefStruct list)
type typedef = typedefStruct list

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)
  | LvalueNone

type binop =
  | Op_add | Op_sub | Op_mul | Op_eq | Op_lt | Op_gt

type unop =
  | Op_minus

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

type paramList = (beantype*string) list

type funcDecParamList = (valRef*beantype*string) list

type stmt = 
  | Assign of (lvalue * (lvalue * rvalue ) list)
  | Read of lvalue
  | Write of expr
  | StmtNone
  | Method of (string * paramList)
  | VarDec of (beantype*string)

type functionDeclaration = (string*funcDecParamList)
(*
type program = {
  typedefs : (typedef*ident) list;
  funcdefs : (functionDeclaration*stmt list) list
}
*)

type program = {
  typedefs : (typedef*ident) list;
  funcdefs : int list
}

type t = program

val printBeantype : beantype -> unit 
val printTypedefStruct : typedefStruct -> unit 
val printTypedefs : (typedefStruct list * ident) list -> unit

