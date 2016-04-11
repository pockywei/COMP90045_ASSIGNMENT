type token =
  | BOOL_VAL of (bool)
  | INT_VAL of (int)
  | STRING_VAL of (string)
  | WRITE
  | READ
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | UMINUS
  | COLON
  | SEMICOLON
  | AND
  | OR
  | NOT
  | EOF
  | IDENTIFIER of (string)
  | LEFT_PAREN
  | RIGHT_PAREN
  | TYPEDEF of (string)
  | TYPEDEF_VALUE_INIT
  | DOT
  | COMMA
  | END
  | VAL
  | REF
  | LEFT_BRACE
  | RIGHT_BRACE
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | BOOL
  | INT
  | PROC
  | EQ_COL

val start_state :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bean_ast.program
