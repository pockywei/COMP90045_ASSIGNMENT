(** The is our lex file it is used to extract tokens for yacc to use
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

(* Ocamllex for getting all the tokens and parse it to Ocamlyacc file *)
{
open Bean_parse

exception LexFail of Lexing.lexbuf
let lex_fail lexbuf = raise (LexFail lexbuf)
}

let apostro = '''
let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let underscore = '_'
let ident = (alpha|underscore)(alpha|underscore|apostro)*
let comment = '#'[^'\n']*'\n'
let string = '"' [^ '"' '\t' '\n' '\r' ]* '"'
let int_val = '-'? digit+

rule token = parse

  (* Whitespace *)
    [' ' '\t']        { Printf.printf "meet space or tab \n" ;flush stdout;token lexbuf }     (* skip blanks *)
  | '\r'?'\n'         { Printf.printf "meet newline\n";flush stdout; Lexing.new_line lexbuf ; token lexbuf }

  (* Constants *)
  | int_val as lxm    { Printf.printf "meet int literal \n" ;flush stdout; INT_VAL(int_of_string(lxm)) }
  | string as lxm     { Printf.printf "meet string => %s \n" lxm ;flush stdout;STRING_VAL(lxm) }
  | "true"            { Printf.printf "meet true \n" ;flush stdout;BOOL_VAL true }
  | "false"           { Printf.printf "meet false \n" ;flush stdout;BOOL_VAL false }

  (* Keywords *)
  | "typedef"         { Printf.printf "meet typedef \n" ;flush stdout;TYPEDEF("typedef") }
  | "proc"            { Printf.printf "meet proc \n ";flush stdout;PROC }
  | "end"             { Printf.printf "meet end \n" ;flush stdout; END }
  | "val"             { Printf.printf "meet val \n" ;flush stdout; VAL }
  | "ref"             { Printf.printf "meet ref \n" ;flush stdout;REF }
  | "while"           { Printf.printf "meet while \n" ;flush stdout;WHILE }
  | "do"              { Printf.printf "meet do \n" ;flush stdout;DO }
  | "od"              { Printf.printf "meet od \n" ;flush stdout;OD }
  | "if"              { Printf.printf "meet if \n" ;flush stdout;IF }
  | "then"            { Printf.printf "meet then \n" ;flush stdout;THEN }
  | "else"            { Printf.printf "meet else \n" ;flush stdout;ELSE }
  | "fi"              { Printf.printf "meet fi \n" ;flush stdout;FI} 
  | "bool"            { Printf.printf "meet bool \n" ;flush stdout;BOOL }
  | "int"             { Printf.printf "meet int \n" ;flush stdout; INT }
  | "read"            { Printf.printf "meet read \n" ;flush stdout;READ }
  | "write"           { Printf.printf "meet write \n" ;flush stdout; WRITE }
  | "and"             { Printf.printf "meet and \n" ;flush stdout;AND }
  | "or"              { Printf.printf "meet or \n" ;flush stdout;OR }
  | "not"             { Printf.printf "meet not \n" ;flush stdout;NOT }

  (* Punctuation *)
  | ':'               { Printf.printf "meet : \n" ;flush stdout;COLON }
  | '{'               { Printf.printf "meet { \n" ;flush stdout;LEFT_BRACE }
  | '}'               { Printf.printf "meet } \n" ;flush stdout;RIGHT_BRACE }
  | ','               { Printf.printf "meet , \n" ;flush stdout;COMMA }
  | '.'               { Printf.printf "meet . \n" ;flush stdout;DOT }
  | '('               { Printf.printf "meet ( \n" ;flush stdout;LEFT_PAREN }
  | ')'               { Printf.printf "meet ) \n" ;flush stdout;RIGHT_PAREN }
  | ';'               { Printf.printf "meet ; \n" ;flush stdout;SEMICOLON }

  (* Symbol Operators *)
  | ":="              { Printf.printf "meet := \n" ;flush stdout;EQ_COL }
  | "!="              { Printf.printf "meet != \n" ;flush stdout;NEQ }
  | '<'               { Printf.printf "meet < \n" ;flush stdout;LT }
  | "<="              { Printf.printf "meet <= \n" ;flush stdout;LTE }
  | '>'               { Printf.printf "meet > \n" ;flush stdout;GT }
  | ">="              { Printf.printf "meet >= \n" ;flush stdout;GTE }
  | '='               { Printf.printf "meet = \n" ;flush stdout;EQ }
  | '+'               { Printf.printf "meet + \n" ;flush stdout;PLUS }
  | '-'               { Printf.printf "meet - \n" ;flush stdout;MINUS }
  | '*'               { Printf.printf "meet * \n" ;flush stdout;MUL }
  | '/'               { Printf.printf "meet / \n" ;flush stdout;DIV }

  (* Miscellaneous *)
  | ident as lxm      { Printf.printf "meet ident => %s\n" lxm ;flush stdout;IDENTIFIER(lxm) }
  | comment           { Printf.printf "Comment \n";flush stdout;token lexbuf }
  | eof               { Printf.printf "end file \n";flush stdout;EOF }
  | _                 { lex_fail lexbuf }