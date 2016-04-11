type token =
  | BOOL_VAL of (bool)
  | INT_VAL of (int)
  | STRING_VAL of (string)
  | WRITE
  | READ
  | ASSIGN
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
  | END
  | VAL
  | REF
  | TYPEDEF of (string)
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
  | AND
  | OR
  | NOT
  | EQ_COL
  | COLON
  | SEMICOLON
  | DOT
  | COMMA
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | EOF
  | IDENTIFIER of (string)

val start_state :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bean_ast.program
