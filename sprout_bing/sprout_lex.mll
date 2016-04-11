{
open Sprout_parse
let line_num = ref 0

exception LexFail of Lexing.lexbuf


let lex_fail lexbuf = raise (LexFail lexbuf)
}

let apostro = '''
let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit (* unused *)
let digits = digit+ (* unused *)
let underscore = '_'
let ident = (alpha|underscore)(alnum|underscore|apostro)* (* to be changed to (alpha|underscore)(alpha|underscore|apostro)* *)
let typedef_value_init = (ident ':' ident) (* unused *)
let function_value_init = ident ident (* unused *)

let comment = '#'[^'\n']*'\n'

rule token = parse
    [' ' '\t']    { Printf.printf "meet space or tab \n" ;flush stdout;token lexbuf }     (* skip blanks *)
  | '\n'          { incr line_num; Printf.printf "meet newline %d\n" !line_num ;flush stdout; Lexing.new_line lexbuf ; token lexbuf }
  (*| '-'?['0'-'9']+ as lxm { IPrintf.printf "meet typedef \n" ;flush stdout;NT_CONST(int_of_string lxm) }*)
  | '-'?['0'-'9']+ as lxm{Printf.printf "meet int literal \n" ;flush stdout; INT_VAL(int_of_string(lxm)) }
  (* keywords *)
  | comment {incr line_num; Printf.printf "Comment \n";flush stdout;token lexbuf }
  | ':' { Printf.printf "meet : \n" ;flush stdout;COLON }
  | '{' { Printf.printf "meet { \n" ;flush stdout;LEFT_BRACE }
  | '}' { Printf.printf "meet } \n" ;flush stdout;RIGHT_BRACE }
  | "typedef" {Printf.printf "meet typedef \n" ;flush stdout;TYPEDEF("typedef") }
  | "proc" {Printf.printf "meet proc \n ";flush stdout;PROC}
  (*| typedef_value_init  as temp{ Printf.printf "meet typedef_value_init => %s \n" temp;flush stdout;TYPEDEF_VALUE_INIT }*)
  | ',' { Printf.printf "meet , \n" ;flush stdout;COMMA }
  | '.' { Printf.printf "meet . \n" ;flush stdout;DOT }
  | "end" {Printf.printf "meet end \n" ;flush stdout; END }
  | "val" {Printf.printf "meet val \n" ;flush stdout; VAL }
  | "ref" { Printf.printf "meet ref \n" ;flush stdout;REF }
  | '(' { Printf.printf "meet ( \n" ;flush stdout;LEFT_PAREN }
  | ')' { Printf.printf "meet ) \n" ;flush stdout;RIGHT_PAREN }
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
  | ":=" {Printf.printf "meet := \n" ;flush stdout;EQ_COL}
  (*| '(' { LPAREN }*)
  (*| ')' { RPAREN }*)
  | "and" { Printf.printf "meet and \n" ;flush stdout;AND }
  | "or" { Printf.printf "meet or \n" ;flush stdout;OR }
  | "not" { Printf.printf "meet not \n" ;flush stdout;NOT }
  | "!=" { Printf.printf "meet != \n" ;flush stdout;NEQ }
  | "<="{ Printf.printf "meet <= \n" ;flush stdout;LTE }
  | ">="{ Printf.printf "meet >= \n" ;flush stdout;GTE }
  | '=' { Printf.printf "meet = \n" ;flush stdout;EQ }
  | '<' { Printf.printf "meet < \n" ;flush stdout;LT }
  | '+' { Printf.printf "meet + \n" ;flush stdout;PLUS }
  | '-' { Printf.printf "meet - \n" ;flush stdout;MINUS }
  | '*' { Printf.printf "meet * \n" ;flush stdout;MUL }
  | '/' { Printf.printf "meet * \n" ;flush stdout;DIV }
  | ';' { Printf.printf "meet ; \n" ;flush stdout;SEMICOLON }
  | '>' { Printf.printf "meet > \n" ;flush stdout;GT}
  | '"' [^ '"' '\t' '\n' '\r' ]* '"' as lxm{ Printf.printf "meet string => %s \n" lxm ;flush stdout;STRING_VAL(lxm)}
  (*| ident as lxm { IDENT lxm }*)
  | ident as lxm{ Printf.printf "meet ident => %s\n" lxm ;flush stdout;IDENTIFIER(lxm) }
  | eof { Printf.printf "end file \n %d \n" !line_num;flush stdout;EOF }
  | _   { lex_fail lexbuf }
