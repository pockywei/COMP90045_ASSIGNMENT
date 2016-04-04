(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int
  | IdentType of string 

type typedef = (ident * beantype)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  | Op_add | Op_sub | Op_mul | Op_eq | Op_lt

type unop =
  | Op_minus

type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

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

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Test
  | Method of string

type program = {
  decls : typedef list list;
  stmts : stmt list list
}
 
type t = program

val print_decls_list : typedef list -> unit 
val print_stmts : stmt list-> unit
