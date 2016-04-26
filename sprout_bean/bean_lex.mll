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

exception LexFail
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
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | '\r'?'\n'         { Lexing.new_line lexbuf ; token lexbuf }

  (* Constants *)
  | int_val as lxm    { INT_VAL(int_of_string(lxm)) }
  | string as lxm     { STRING_VAL(lxm) }
  | "true"            { BOOL_VAL true }
  | "false"           { BOOL_VAL false }

  (* Keywords *)
  | "typedef"         { TYPEDEF("typedef") }
  | "proc"            { PROC }
  | "end"             { END }
  | "val"             { VAL }
  | "ref"             { REF }
  | "while"           { WHILE }
  | "do"              { DO }
  | "od"              { OD }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "fi"              { FI } 
  | "bool"            { BOOL }
  | "int"             { INT }
  | "read"            { READ }
  | "write"           { WRITE }
  | "and"             { AND }
  | "or"              { OR }
  | "not"             { NOT }

  (* Punctuation *)
  | ':'               { COLON }
  | '{'               { LEFT_BRACE }
  | '}'               { RIGHT_BRACE }
  | ','               { COMMA }
  | '.'               { DOT }
  | '('               { LEFT_PAREN }
  | ')'               { RIGHT_PAREN }
  | ';'               { SEMICOLON }

  (* Symbol Operators *)
  | ":="              { EQ_COL }
  | "!="              { NEQ }
  | '<'               { LT }
  | "<="              { LTE }
  | '>'               { GT }
  | ">="              { GTE }
  | '='               { EQ }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { MUL }
  | '/'               { DIV }

  (* Miscellaneous *)
  | ident as lxm      { IDENTIFIER(lxm) }
  | comment           { token lexbuf }
  | eof               { EOF }
  | _                 { raise LexFail }