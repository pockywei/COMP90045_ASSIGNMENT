{
open Sprout_parse
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit
let digits = digit+
let underscore = '_'
let ident = (alpha alnum* underscore?)+
let typedef_value_init = (ident ':' ident)
let function_value_init = ident ident

rule token = parse
    [' ' '\t']    { Printf.printf "meet space or tab \n" ;flush stdout;token lexbuf }     (* skip blanks *)
  | '\n'          {Printf.printf "meet newline \n" ;flush stdout; Lexing.new_line lexbuf ; token lexbuf }
  (*| '-'?['0'-'9']+ as lxm { IPrintf.printf "meet typedef \n" ;flush stdout;NT_CONST(int_of_string lxm) }*)
  | '-'?['0'-'9']+ as lxm{Printf.printf "meet int literal  %s \n" lxm ;flush stdout; INT_VAL(int_of_string(lxm)) }
  (* keywords *)
  | ':' { Printf.printf "meet : \n" ;flush stdout;COLON }
  | '{' { Printf.printf "meet { \n" ;flush stdout;LEFT_PARENTHESIS }
  | '}' { Printf.printf "meet } \n" ;flush stdout;RIGHT_PARENTHESIS }
  | "typedef" {Printf.printf "meet typedef \n" ;flush stdout;TYPEDEF("typedef") }
  | "proc" {Printf.printf "meet proc \n ";flush stdout;PROC}
  (*| typedef_value_init  as temp{ Printf.printf "meet typedef_value_init => %s \n" temp;flush stdout;TYPEDEF_VALUE_INIT }*)
  | ',' { Printf.printf "meet , \n" ;flush stdout;COMMA }
  | '.' { Printf.printf "meet . \n" ;flush stdout;DOT }
  | "end" {Printf.printf "meet end \n" ;flush stdout; END }
  | "val" {Printf.printf "meet val \n" ;flush stdout; VAL }
  | "ref" { Printf.printf "meet ref \n" ;flush stdout;REF }
  | '(' { Printf.printf "meet ( \n" ;flush stdout;LEFT_BRACKET }
  | ')' { Printf.printf "meet ) \n" ;flush stdout;RIGHT_BRACKET }
  | "while" { Printf.printf "meet while \n" ;flush stdout;WHILE }
  | "do" { Printf.printf "meet do \n" ;flush stdout;DO }
  | "od" { Printf.printf "meet od \n" ;flush stdout;OD }
  | "if" { Printf.printf "meet if \n" ;flush stdout;IF }
  | "then" { Printf.printf "meet then \n" ;flush stdout;THEN }
  | "else" { Printf.printf "meet else \n" ;flush stdout;ELSE }
  | "fi" {Printf.printf "meet fi \n" ;flush stdout;FI} 
  | "bool" { Printf.printf "meet bool \n" ;flush stdout;BOOL }
  | "int" { Printf.printf "meet int \n" ;flush stdout; INT }
  (*| "true" { Printf.printf "meet typedef \n" ;flush stdout;BOOL_VAL true }*)
  | "true" {Printf.printf "meet true \n" ;flush stdout;BOOL_VAL true}
  (*| "false" { Printf.printf "meet typedef \n" ;flush stdout;BOOL_VAL false }*)
  | "false" {Printf.printf "meet false \n" ;flush stdout;BOOL_VAL false}
  | "read" { Printf.printf "meet read \n" ;flush stdout;READ }
  | "write" {Printf.printf "meet write \n" ;flush stdout; WRITE }
  (*| ":=" { Printf.printf "meet typedef \n" ;flush stdout;ASSIGN }*)
  | ":=" {Printf.printf "meet := \n" ;flush stdout;EQ_DOT}
  (*| '(' { LPAREN }*)
  (*| ')' { RPAREN }*)
  | '=' { Printf.printf "meet = \n" ;flush stdout;EQ }
  | '<' { Printf.printf "meet < \n" ;flush stdout;LT }
  | '+' { Printf.printf "meet + \n" ;flush stdout;PLUS }
  | '-' { Printf.printf "meet - \n" ;flush stdout;MINUS }
  | '*' { Printf.printf "meet * \n" ;flush stdout;MUL }
  | ';' { Printf.printf "meet ; \n" ;flush stdout;SEMICOLON }
  | '>' { Printf.printf "meet > \n" ;flush stdout;GT}
  (*| ident as lxm { IDENT lxm }*)
  | ident as lxm{ Printf.printf "meet ident => %s\n" lxm ;flush stdout;IDENTIFIER(lxm) }
  | eof { Printf.printf "end file \n";flush stdout;EOF }
