(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int

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

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Test

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program

let print_decls_list ls = List.iter ( fun x -> match x with
  | (m,n) -> match n with 
      | Bool -> Printf.printf "(Bool , %s)  " m
      | Int -> Printf.printf " (Int , %s)  " m ;) ls

let print_stmts ls = List.iter (fun x -> match x with
  | Assign _  ->  Printf.printf "Assignment "
  | Read _-> Printf.printf "Read "
  | Write _-> Printf.printf "Write "
  | Test -> Printf.printf "test stmts") ls