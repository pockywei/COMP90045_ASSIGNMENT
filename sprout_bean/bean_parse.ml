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

open Parsing;;
let _ = parse_error;;
# 22 "bean_parse.mly"
open Bean_ast
# 53 "bean_parse.ml"
let yytransl_const = [|
  260 (* WRITE *);
  261 (* READ *);
  262 (* ASSIGN *);
  263 (* WHILE *);
  264 (* DO *);
  265 (* OD *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
  269 (* FI *);
  270 (* BOOL *);
  271 (* INT *);
  272 (* PROC *);
  273 (* END *);
  274 (* VAL *);
  275 (* REF *);
  277 (* EQ *);
  278 (* NEQ *);
  279 (* LT *);
  280 (* LTE *);
  281 (* GT *);
  282 (* GTE *);
  283 (* PLUS *);
  284 (* MINUS *);
  285 (* MUL *);
  286 (* DIV *);
  287 (* UMINUS *);
  288 (* AND *);
  289 (* OR *);
  290 (* NOT *);
  291 (* EQ_COL *);
  292 (* COLON *);
  293 (* SEMICOLON *);
  294 (* DOT *);
  295 (* COMMA *);
  296 (* LEFT_PAREN *);
  297 (* RIGHT_PAREN *);
  298 (* LEFT_BRACE *);
  299 (* RIGHT_BRACE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_VAL *);
  258 (* INT_VAL *);
  259 (* STRING_VAL *);
  276 (* TYPEDEF *);
  300 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\005\000\005\000\
\006\000\007\000\007\000\003\000\008\000\008\000\009\000\012\000\
\012\000\013\000\013\000\014\000\014\000\010\000\010\000\011\000\
\011\000\016\000\016\000\015\000\015\000\015\000\015\000\015\000\
\017\000\017\000\018\000\018\000\019\000\019\000\019\000\023\000\
\024\000\024\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\021\000\026\000\026\000\022\000\022\000\
\025\000\025\000\025\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\001\000\001\000\003\000\001\000\001\000\
\004\000\002\000\000\000\006\000\001\000\000\000\004\000\004\000\
\000\000\002\000\000\000\001\000\001\000\004\000\000\000\003\000\
\002\000\001\000\000\000\003\000\002\000\002\000\004\000\003\000\
\006\000\005\000\001\000\003\000\001\000\003\000\002\000\004\000\
\002\000\000\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000\002\000\000\000\002\000\000\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\068\000\000\000\000\000\000\000\000\000\007\000\
\008\000\011\000\005\000\000\000\004\000\000\000\000\000\000\000\
\002\000\000\000\023\000\010\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\018\000\015\000\
\020\000\021\000\000\000\065\000\066\000\067\000\000\000\000\000\
\000\000\035\000\000\000\000\000\044\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\026\000\025\000\000\000\
\000\000\000\000\059\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\022\000\024\000\
\000\000\028\000\000\000\036\000\016\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\049\000\
\000\000\000\000\000\000\000\000\000\000\060\000\031\000\039\000\
\000\000\000\000\034\000\000\000\000\000\061\000\041\000\038\000\
\000\000\063\000\033\000\000\000\040\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\012\000\013\000\015\000\016\000\007\000\
\019\000\024\000\062\000\026\000\027\000\043\000\035\000\063\000\
\036\000\051\000\090\000\085\000\086\000\117\000\113\000\114\000\
\053\000\110\000"

let yysindex = "\023\000\
\000\000\000\000\000\000\059\255\012\255\000\000\043\255\000\000\
\000\000\000\000\000\000\038\255\000\000\041\255\221\254\054\255\
\000\000\047\255\000\000\000\000\000\000\053\255\000\000\076\255\
\012\255\240\254\039\255\075\255\057\255\075\255\075\255\062\255\
\060\255\088\255\070\255\028\255\232\254\000\000\000\000\000\000\
\000\000\000\000\012\255\000\000\000\000\000\000\075\255\075\255\
\075\255\000\000\079\255\117\000\000\000\079\255\044\000\064\000\
\027\255\073\255\000\000\028\255\062\255\000\000\000\000\072\255\
\069\255\091\255\000\000\142\000\077\000\075\255\075\255\075\255\
\075\255\075\255\075\255\075\255\075\255\075\255\075\255\075\255\
\075\255\028\255\028\255\000\000\098\000\095\255\000\000\000\000\
\097\255\000\000\117\000\000\000\000\000\000\000\065\255\065\255\
\065\255\065\255\065\255\065\255\040\255\040\255\000\000\000\000\
\142\000\130\000\129\255\130\255\075\255\000\000\000\000\000\000\
\045\255\108\255\000\000\028\255\140\255\000\000\000\000\000\000\
\137\255\000\000\000\000\072\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\043\255\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\227\254\
\000\000\000\000\000\000\179\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\100\255\122\255\000\000\124\255\000\000\000\000\
\000\000\000\000\000\000\179\255\255\254\000\000\000\000\000\000\
\000\000\000\000\000\000\169\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\134\255\000\000\000\000\000\000\
\119\255\000\000\235\254\000\000\000\000\000\000\182\255\196\255\
\209\255\223\255\243\255\003\000\123\255\146\255\000\000\000\000\
\021\000\004\000\000\000\152\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\244\255\000\000\000\000\000\000\000\000\
\000\000\000\000\239\255\000\000\000\000\000\000\000\000\116\000\
\000\000\237\255\057\000\228\255\073\000\000\000\000\000\000\000\
\000\000\000\000"

let yytablesize = 428
let yytable = "\052\000\
\001\000\055\000\056\000\020\000\037\000\035\000\034\000\021\000\
\035\000\054\000\064\000\033\000\038\000\065\000\005\000\037\000\
\037\000\037\000\067\000\068\000\069\000\037\000\039\000\001\000\
\040\000\008\000\009\000\044\000\045\000\046\000\066\000\028\000\
\029\000\035\000\030\000\091\000\035\000\031\000\019\000\019\000\
\037\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\010\000\047\000\011\000\
\041\000\042\000\014\000\017\000\048\000\017\000\037\000\037\000\
\107\000\108\000\049\000\084\000\078\000\079\000\050\000\061\000\
\044\000\045\000\046\000\044\000\045\000\046\000\005\000\028\000\
\029\000\017\000\030\000\119\000\018\000\031\000\023\000\120\000\
\025\000\008\000\009\000\076\000\077\000\078\000\079\000\091\000\
\037\000\022\000\122\000\047\000\050\000\057\000\047\000\058\000\
\059\000\048\000\060\000\043\000\048\000\087\000\043\000\049\000\
\092\000\089\000\049\000\050\000\065\000\010\000\050\000\032\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\046\000\043\000\043\000\046\000\093\000\111\000\
\043\000\115\000\043\000\112\000\043\000\116\000\043\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\121\000\
\123\000\047\000\046\000\046\000\047\000\124\000\030\000\046\000\
\029\000\046\000\042\000\046\000\064\000\046\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\062\000\088\000\
\058\000\047\000\047\000\058\000\125\000\118\000\047\000\000\000\
\047\000\000\000\047\000\027\000\047\000\050\000\027\000\027\000\
\050\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\058\000\058\000\000\000\051\000\000\000\058\000\051\000\058\000\
\000\000\058\000\000\000\058\000\000\000\050\000\050\000\000\000\
\052\000\000\000\050\000\052\000\050\000\000\000\050\000\000\000\
\050\000\000\000\000\000\051\000\051\000\000\000\054\000\000\000\
\051\000\054\000\051\000\000\000\051\000\000\000\051\000\000\000\
\052\000\052\000\000\000\000\000\000\000\052\000\000\000\052\000\
\000\000\052\000\053\000\052\000\000\000\053\000\054\000\054\000\
\000\000\000\000\000\000\054\000\000\000\054\000\000\000\054\000\
\000\000\054\000\055\000\057\000\000\000\055\000\057\000\000\000\
\013\000\000\000\053\000\053\000\000\000\000\000\000\000\053\000\
\000\000\053\000\000\000\053\000\056\000\053\000\000\000\056\000\
\000\000\000\000\055\000\055\000\057\000\000\000\000\000\055\000\
\057\000\055\000\057\000\055\000\057\000\055\000\057\000\000\000\
\000\000\000\000\000\000\082\000\056\000\056\000\000\000\000\000\
\000\000\056\000\000\000\056\000\000\000\056\000\000\000\056\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\083\000\080\000\081\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\000\000\080\000\
\081\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\000\000\080\000\081\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\094\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\000\000\080\000\081\000\000\000\000\000\000\000\000\000\000\000\
\109\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\000\000\080\000\081\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\000\000\080\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000"

let yycheck = "\028\000\
\000\000\030\000\031\000\039\001\024\000\035\001\024\000\043\001\
\038\001\029\000\035\001\024\000\025\000\038\001\044\001\037\001\
\036\000\039\001\047\000\048\000\049\000\043\001\039\001\001\000\
\041\001\014\001\015\001\001\001\002\001\003\001\043\000\004\001\
\005\001\035\001\007\001\064\000\038\001\010\001\018\001\019\001\
\060\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\042\001\028\001\044\001\
\018\001\019\001\016\001\039\001\034\001\041\001\082\000\083\000\
\082\000\083\000\040\001\041\001\029\001\030\001\044\001\044\001\
\001\001\002\001\003\001\001\001\002\001\003\001\020\001\004\001\
\005\001\044\001\007\001\039\001\044\001\010\001\040\001\043\001\
\036\001\014\001\015\001\027\001\028\001\029\001\030\001\124\000\
\116\000\044\001\116\000\028\001\044\001\040\001\028\001\044\001\
\017\001\034\001\037\001\008\001\034\001\037\001\011\001\040\001\
\044\001\042\001\040\001\044\001\038\001\042\001\044\001\044\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\008\001\032\001\033\001\011\001\044\001\041\001\
\037\001\009\001\039\001\043\001\041\001\012\001\043\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\044\001\
\013\001\008\001\032\001\033\001\011\001\021\001\037\001\037\001\
\037\001\039\001\044\001\041\001\013\001\043\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\041\001\060\000\
\008\001\032\001\033\001\011\001\124\000\109\000\037\001\255\255\
\039\001\255\255\041\001\009\001\043\001\008\001\012\001\013\001\
\011\001\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\032\001\033\001\255\255\008\001\255\255\037\001\011\001\039\001\
\255\255\041\001\255\255\043\001\255\255\032\001\033\001\255\255\
\008\001\255\255\037\001\011\001\039\001\255\255\041\001\255\255\
\043\001\255\255\255\255\032\001\033\001\255\255\008\001\255\255\
\037\001\011\001\039\001\255\255\041\001\255\255\043\001\255\255\
\032\001\033\001\255\255\255\255\255\255\037\001\255\255\039\001\
\255\255\041\001\008\001\043\001\255\255\011\001\032\001\033\001\
\255\255\255\255\255\255\037\001\255\255\039\001\255\255\041\001\
\255\255\043\001\008\001\008\001\255\255\011\001\011\001\255\255\
\016\001\255\255\032\001\033\001\255\255\255\255\255\255\037\001\
\255\255\039\001\255\255\041\001\008\001\043\001\255\255\011\001\
\255\255\255\255\032\001\033\001\033\001\255\255\255\255\037\001\
\037\001\039\001\039\001\041\001\041\001\043\001\043\001\255\255\
\255\255\255\255\255\255\008\001\032\001\033\001\255\255\255\255\
\255\255\037\001\255\255\039\001\255\255\041\001\255\255\043\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\011\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\255\255\032\001\
\033\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\255\255\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\041\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\039\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\255\255\032\001\033\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001"

let yynames_const = "\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  WHILE\000\
  DO\000\
  OD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  BOOL\000\
  INT\000\
  PROC\000\
  END\000\
  VAL\000\
  REF\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  UMINUS\000\
  AND\000\
  OR\000\
  NOT\000\
  EQ_COL\000\
  COLON\000\
  SEMICOLON\000\
  DOT\000\
  COMMA\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_VAL\000\
  INT_VAL\000\
  STRING_VAL\000\
  TYPEDEF\000\
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 85 "bean_parse.mly"
                                       ({typedefs = List.rev _1;funcdefs = List.rev _2})
# 365 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "bean_parse.mly"
                                               ((_3,_4)::_1)
# 375 "bean_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "bean_parse.mly"
  ([])
# 381 "bean_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive_type) in
    Obj.repr(
# 117 "bean_parse.mly"
                 (_1)
# 388 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "bean_parse.mly"
             (SingleTypeTerm((IdentType _1)))
# 395 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 119 "bean_parse.mly"
                                          (ListTypeTerm( List.rev _2))
# 402 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "bean_parse.mly"
       (SingleTypeTerm(Bool))
# 408 "bean_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "bean_parse.mly"
      (SingleTypeTerm(Int))
# 414 "bean_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_spec) in
    Obj.repr(
# 139 "bean_parse.mly"
                                                  (SingleTypeTermWithIdent(_2,_4)::_1)
# 423 "bean_parse.ml"
               : 'field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 143 "bean_parse.mly"
                         (_1)
# 430 "bean_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "bean_parse.mly"
  ([])
# 436 "bean_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rec_procedure_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'procedure_header) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_definition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 149 "bean_parse.mly"
                                                                                   ((_3,List.rev _4,_5)::_1)
# 446 "bean_parse.ml"
               : 'procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 152 "bean_parse.mly"
                       (_1)
# 453 "bean_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "bean_parse.mly"
  ([])
# 459 "bean_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 161 "bean_parse.mly"
                                          ((_1,List.rev _3))
# 467 "bean_parse.ml"
               : 'procedure_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_param) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'param_passing_indicator) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "bean_parse.mly"
                                                         ((_2,_3,_4)::_1)
# 477 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "bean_parse.mly"
  ([])
# 483 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 171 "bean_parse.mly"
              (_1)
# 490 "bean_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "bean_parse.mly"
  ([])
# 496 "bean_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "bean_parse.mly"
      (Val)
# 502 "bean_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "bean_parse.mly"
      (Ref)
# 508 "bean_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 182 "bean_parse.mly"
                                                     ( SingleTypeTermWithIdent(_3,_2)::_1 )
# 517 "bean_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "bean_parse.mly"
  ([])
# 523 "bean_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rec_stmt_list) in
    Obj.repr(
# 190 "bean_parse.mly"
                                      (_1::_3)
# 531 "bean_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'compound_stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rec_stmt_list) in
    Obj.repr(
# 191 "bean_parse.mly"
                              (_1::_2)
# 539 "bean_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 195 "bean_parse.mly"
            (_1)
# 546 "bean_parse.ml"
               : 'rec_stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "bean_parse.mly"
  ([])
# 552 "bean_parse.ml"
               : 'rec_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 201 "bean_parse.mly"
                       ( Assign(_1,_3))
# 560 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 202 "bean_parse.mly"
              ( Read(_2) )
# 567 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 203 "bean_parse.mly"
             ( Write(_2) )
# 574 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 204 "bean_parse.mly"
                                              ( Method(_1,_3) )
# 582 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 205 "bean_parse.mly"
                                    ( Method(_1,[]) )
# 589 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_block) in
    Obj.repr(
# 210 "bean_parse.mly"
                                       (IfDec(_2,_4,_5))
# 598 "bean_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 211 "bean_parse.mly"
                             (WhileDec(_2,_4))
# 606 "bean_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 216 "bean_parse.mly"
             ( LId(_1) )
# 613 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 217 "bean_parse.mly"
                        ( LField(_1,_3) )
# 621 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 221 "bean_parse.mly"
       ( Rexpr(_1) )
# 628 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 222 "bean_parse.mly"
                                    ( Rstmts(List.rev _2) )
# 635 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 223 "bean_parse.mly"
                         ( Rempty )
# 641 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_init) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 227 "bean_parse.mly"
                                      (Rassign(_2,_4)::_1)
# 650 "bean_parse.ml"
               : 'field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 230 "bean_parse.mly"
                   (_1)
# 657 "bean_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    Obj.repr(
# 231 "bean_parse.mly"
  ([])
# 663 "bean_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 235 "bean_parse.mly"
         ( Elval(_1) )
# 670 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 236 "bean_parse.mly"
        ( _1 )
# 677 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 237 "bean_parse.mly"
                              ( Ebracket(_2) )
# 684 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 238 "bean_parse.mly"
                 ( Ebinop(_1,Op_add,_3) )
# 692 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 239 "bean_parse.mly"
                  ( Ebinop(_1,Op_sub,_3) )
# 700 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 240 "bean_parse.mly"
                ( Ebinop(_1,Op_mul,_3) )
# 708 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 241 "bean_parse.mly"
                ( Ebinop(_1,Op_div,_3) )
# 716 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 242 "bean_parse.mly"
               ( Ebinop(_1,Op_eq,_3) )
# 724 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 243 "bean_parse.mly"
                ( Ebinop(_1,Op_neq,_3) )
# 732 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 244 "bean_parse.mly"
               ( Ebinop(_1,Op_lt,_3) )
# 740 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 245 "bean_parse.mly"
               ( Ebinop(_1,Op_gt,_3) )
# 748 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 246 "bean_parse.mly"
                ( Ebinop(_1,Op_lte,_3) )
# 756 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 247 "bean_parse.mly"
                ( Ebinop(_1,Op_gte,_3) )
# 764 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 248 "bean_parse.mly"
                ( Ebinop(_1,Op_and,_3) )
# 772 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 249 "bean_parse.mly"
               ( Ebinop(_1,Op_or,_3) )
# 780 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 250 "bean_parse.mly"
           ( Eunop(Op_not,_2) )
# 787 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 251 "bean_parse.mly"
                          ( Eunop(Op_minus,_2) )
# 794 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rec_expr_list) in
    Obj.repr(
# 255 "bean_parse.mly"
                     ( _1::_2 )
# 802 "bean_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 259 "bean_parse.mly"
                  ( _2 )
# 809 "bean_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 260 "bean_parse.mly"
  ([])
# 815 "bean_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 264 "bean_parse.mly"
                 (_2)
# 822 "bean_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 265 "bean_parse.mly"
  ([])
# 828 "bean_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 269 "bean_parse.mly"
           ( Ebool(_1) )
# 835 "bean_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 270 "bean_parse.mly"
          ( Eint(_1) )
# 842 "bean_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 271 "bean_parse.mly"
             ( Eident(_1) )
# 849 "bean_parse.ml"
               : 'const))
(* Entry start_state *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start_state (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Bean_ast.program)
