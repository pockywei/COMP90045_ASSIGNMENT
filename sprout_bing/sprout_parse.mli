type token =
  | BOOL_VAL of (bool)
  | INT_VAL of (int)
  | IDENT of (string)
  | WRITE
  | READ
  | ASSIGN
  | LPAREN
  | RPAREN
  | EQ
  | LT
  | GT
  | PLUS
  | MINUS
  | MUL
  | SEMICOLON
  | EOF
  | COLON
  | IDENTIFIER of (string)
  | LEFT_PARENTHESIS
  | RIGHT_PARENTHESIS
  | TYPEDEF of (string)
  | TYPEDEF_VALUE_INIT
  | DOT
  | COMMA
  | END
  | VAL
  | REF
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | BOOL
  | INT
  | EQ_DOT
  | PROC

val start_state :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sprout_ast.program
