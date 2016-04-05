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

open Parsing;;
let _ = parse_error;;
# 3 "sprout_parse.mly"
open Sprout_ast
# 54 "sprout_parse.ml"
let yytransl_const = [|
  260 (* WRITE *);
  261 (* READ *);
  262 (* ASSIGN *);
  263 (* EQ *);
  264 (* NEQ *);
  265 (* LT *);
  266 (* LTE *);
  267 (* GT *);
  268 (* GTE *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* MUL *);
  272 (* DIV *);
  273 (* UMINUS *);
  274 (* COLON *);
  275 (* SEMICOLON *);
  276 (* AND *);
  277 (* OR *);
  278 (* NOT *);
    0 (* EOF *);
  280 (* LEFT_PAREN *);
  281 (* RIGHT_PAREN *);
  283 (* TYPEDEF_VALUE_INIT *);
  284 (* DOT *);
  285 (* COMMA *);
  286 (* END *);
  287 (* VAL *);
  288 (* REF *);
  289 (* LEFT_BRACE *);
  290 (* RIGHT_BRACE *);
  291 (* WHILE *);
  292 (* DO *);
  293 (* OD *);
  294 (* IF *);
  295 (* THEN *);
  296 (* ELSE *);
  297 (* FI *);
  298 (* BOOL *);
  299 (* INT *);
  300 (* PROC *);
  301 (* EQ_COL *);
    0|]

let yytransl_block = [|
  257 (* BOOL_VAL *);
  258 (* INT_VAL *);
  259 (* STRING_VAL *);
  279 (* IDENTIFIER *);
  282 (* TYPEDEF *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\005\000\005\000\
\006\000\007\000\007\000\003\000\008\000\008\000\009\000\012\000\
\012\000\013\000\013\000\014\000\014\000\010\000\010\000\011\000\
\011\000\015\000\015\000\016\000\016\000\016\000\016\000\017\000\
\017\000\018\000\018\000\019\000\019\000\024\000\025\000\025\000\
\020\000\020\000\020\000\020\000\020\000\021\000\021\000\029\000\
\029\000\022\000\023\000\023\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\028\000\028\000\026\000\026\000\026\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\001\000\001\000\003\000\001\000\001\000\
\004\000\002\000\000\000\006\000\001\000\000\000\004\000\004\000\
\000\000\002\000\000\000\001\000\001\000\004\000\000\000\003\000\
\002\000\001\000\000\000\003\000\002\000\002\000\004\000\006\000\
\005\000\001\000\003\000\001\000\003\000\004\000\002\000\000\000\
\001\000\001\000\003\000\003\000\002\000\002\000\000\000\002\000\
\000\000\001\000\002\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\070\000\000\000\000\000\000\000\000\000\005\000\
\011\000\007\000\008\000\000\000\004\000\000\000\000\000\000\000\
\002\000\000\000\023\000\010\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\015\000\
\018\000\020\000\021\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\022\000\
\067\000\068\000\069\000\066\000\065\000\034\000\000\000\000\000\
\000\000\042\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\000\000\016\000\000\000\057\000\058\000\059\000\061\000\
\060\000\062\000\053\000\054\000\055\000\056\000\063\000\064\000\
\000\000\000\000\000\000\000\000\000\000\027\000\027\000\035\000\
\040\000\028\000\000\000\043\000\000\000\048\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\027\000\000\000\
\039\000\037\000\000\000\051\000\032\000\000\000\038\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\012\000\013\000\015\000\016\000\007\000\
\019\000\024\000\097\000\026\000\027\000\036\000\030\000\044\000\
\045\000\056\000\090\000\091\000\084\000\098\000\104\000\100\000\
\101\000\058\000\081\000\059\000\085\000"

let yysindex = "\026\000\
\000\000\000\000\000\000\232\254\131\255\000\000\247\254\000\000\
\000\000\000\000\000\000\027\255\000\000\053\255\043\255\073\255\
\000\000\074\255\000\000\000\000\000\000\108\255\000\000\131\255\
\131\255\022\255\037\255\106\255\067\255\001\255\000\000\000\000\
\000\000\000\000\000\000\131\255\111\255\000\000\008\255\109\255\
\107\255\008\255\008\255\115\255\000\000\240\254\124\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\255\120\255\
\187\255\000\000\008\255\120\255\008\255\099\255\079\255\000\000\
\128\255\100\255\000\000\168\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\255\187\255\129\255\130\255\008\255\000\000\000\000\000\000\
\000\000\000\000\187\255\000\000\187\255\000\000\000\000\187\255\
\000\000\116\255\117\255\049\255\136\255\000\000\000\000\119\255\
\000\000\000\000\155\255\000\000\000\000\100\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\247\254\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\096\255\014\255\
\000\000\000\000\000\000\000\000\010\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\245\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\255\
\146\255\000\000\000\000\148\255\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\250\254\000\000\127\255\000\000\000\000\144\255\
\003\255\000\000\145\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\080\000\000\000\000\000\000\000\000\000\
\000\000\000\000\146\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\061\000\217\255\000\000\190\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 301
let yytable = "\057\000\
\001\000\005\000\062\000\063\000\039\000\040\000\026\000\026\000\
\049\000\050\000\051\000\065\000\036\000\026\000\026\000\068\000\
\034\000\027\000\027\000\082\000\099\000\083\000\036\000\041\000\
\052\000\026\000\001\000\036\000\066\000\053\000\054\000\055\000\
\026\000\034\000\014\000\042\000\108\000\026\000\043\000\050\000\
\026\000\093\000\050\000\050\000\026\000\096\000\032\000\026\000\
\027\000\017\000\033\000\027\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\046\000\045\000\
\041\000\041\000\041\000\034\000\035\000\045\000\041\000\020\000\
\060\000\045\000\041\000\018\000\021\000\105\000\045\000\041\000\
\045\000\041\000\106\000\045\000\041\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\022\000\
\038\000\023\000\079\000\080\000\049\000\050\000\051\000\028\000\
\031\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\047\000\052\000\087\000\079\000\080\000\
\017\000\053\000\054\000\055\000\017\000\025\000\019\000\019\000\
\037\000\048\000\061\000\054\000\089\000\064\000\086\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\044\000\067\000\065\000\079\000\080\000\088\000\044\000\
\102\000\008\000\095\000\044\000\103\000\094\000\107\000\109\000\
\044\000\110\000\044\000\009\000\030\000\044\000\029\000\047\000\
\046\000\029\000\111\000\000\000\010\000\011\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\000\000\052\000\000\000\079\000\080\000\000\000\000\000\000\000\
\092\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\000\000\000\000\000\000\079\000\080\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000"

let yycheck = "\039\000\
\000\000\026\001\042\000\043\000\004\001\005\001\004\001\005\001\
\001\001\002\001\003\001\028\001\019\001\004\001\005\001\055\000\
\028\001\004\001\005\001\059\000\087\000\061\000\029\001\023\001\
\017\001\023\001\001\000\034\001\045\001\022\001\023\001\024\001\
\023\001\045\001\044\001\035\001\103\000\035\001\038\001\037\001\
\038\001\081\000\040\001\041\001\035\001\085\000\025\001\038\001\
\035\001\023\001\029\001\038\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\030\000\019\001\
\019\001\020\001\021\001\031\001\032\001\025\001\025\001\029\001\
\040\000\029\001\029\001\023\001\034\001\029\001\034\001\034\001\
\036\001\036\001\034\001\039\001\039\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\023\001\
\030\001\024\001\020\001\021\001\001\001\002\001\003\001\024\000\
\025\000\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\036\000\017\001\039\001\020\001\021\001\
\025\001\022\001\023\001\024\001\029\001\018\001\031\001\032\001\
\023\001\019\001\024\001\023\001\033\001\019\001\036\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\019\001\023\001\028\001\020\001\021\001\023\001\025\001\
\037\001\023\001\025\001\029\001\040\001\029\001\023\001\041\001\
\034\001\007\001\036\001\033\001\019\001\039\001\019\001\025\001\
\025\001\024\000\110\000\255\255\042\001\043\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\041\001\255\255\020\001\021\001\255\255\255\255\255\255\
\025\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\044\001"

let yynames_const = "\
  WRITE\000\
  READ\000\
  ASSIGN\000\
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
  COLON\000\
  SEMICOLON\000\
  AND\000\
  OR\000\
  NOT\000\
  EOF\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  TYPEDEF_VALUE_INIT\000\
  DOT\000\
  COMMA\000\
  END\000\
  VAL\000\
  REF\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
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
  EQ_COL\000\
  "

let yynames_block = "\
  BOOL_VAL\000\
  INT_VAL\000\
  STRING_VAL\000\
  IDENTIFIER\000\
  TYPEDEF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 51 "sprout_parse.mly"
                                       ({typedefs=[1];funcdefs=[2]})
# 330 "sprout_parse.ml"
               : Sprout_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "sprout_parse.mly"
                                               ()
# 340 "sprout_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "sprout_parse.mly"
  ()
# 346 "sprout_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive_type) in
    Obj.repr(
# 58 "sprout_parse.mly"
                 ()
# 353 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "sprout_parse.mly"
             ()
# 360 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 60 "sprout_parse.mly"
                                          ()
# 367 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "sprout_parse.mly"
       ()
# 373 "sprout_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "sprout_parse.mly"
      ()
# 379 "sprout_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_spec) in
    Obj.repr(
# 67 "sprout_parse.mly"
                                                  ()
# 388 "sprout_parse.ml"
               : 'field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 70 "sprout_parse.mly"
                         ()
# 395 "sprout_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "sprout_parse.mly"
  ()
# 401 "sprout_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rec_procedure_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'procedure_header) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_definition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'procedure_body) in
    Obj.repr(
# 75 "sprout_parse.mly"
                                                                                        ()
# 411 "sprout_parse.ml"
               : 'procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 78 "sprout_parse.mly"
                       ()
# 418 "sprout_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "sprout_parse.mly"
  ()
# 424 "sprout_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 82 "sprout_parse.mly"
                                          ()
# 432 "sprout_parse.ml"
               : 'procedure_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_param) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'param_passing_indicator) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "sprout_parse.mly"
                                                         ()
# 442 "sprout_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "sprout_parse.mly"
  ()
# 448 "sprout_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 89 "sprout_parse.mly"
              ()
# 455 "sprout_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "sprout_parse.mly"
  ()
# 461 "sprout_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "sprout_parse.mly"
      ()
# 467 "sprout_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "sprout_parse.mly"
      ()
# 473 "sprout_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "sprout_parse.mly"
                                                    ()
# 482 "sprout_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "sprout_parse.mly"
  ()
# 488 "sprout_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rec_procedure_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_stmt) in
    Obj.repr(
# 101 "sprout_parse.mly"
                                           ()
# 496 "sprout_parse.ml"
               : 'procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rec_procedure_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compound_stmt) in
    Obj.repr(
# 102 "sprout_parse.mly"
                                   ()
# 504 "sprout_parse.ml"
               : 'procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_body) in
    Obj.repr(
# 105 "sprout_parse.mly"
                 ()
# 511 "sprout_parse.ml"
               : 'rec_procedure_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "sprout_parse.mly"
  ()
# 517 "sprout_parse.ml"
               : 'rec_procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 109 "sprout_parse.mly"
                       ()
# 525 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 110 "sprout_parse.mly"
              ()
# 532 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "sprout_parse.mly"
             ()
# 539 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 112 "sprout_parse.mly"
                                              ()
# 547 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_block) in
    Obj.repr(
# 115 "sprout_parse.mly"
                                       ()
# 556 "sprout_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 116 "sprout_parse.mly"
                             ()
# 564 "sprout_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "sprout_parse.mly"
             ()
# 571 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "sprout_parse.mly"
                        ()
# 579 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "sprout_parse.mly"
       ()
# 586 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 124 "sprout_parse.mly"
                                    ()
# 593 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_init) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 127 "sprout_parse.mly"
                                      ()
# 602 "sprout_parse.ml"
               : 'field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 130 "sprout_parse.mly"
                   ()
# 609 "sprout_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "sprout_parse.mly"
  ()
# 615 "sprout_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 134 "sprout_parse.mly"
         ()
# 622 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 135 "sprout_parse.mly"
        ()
# 629 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "sprout_parse.mly"
                              ()
# 636 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "sprout_parse.mly"
                  ()
# 645 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unop) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "sprout_parse.mly"
            ()
# 653 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rec_expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "sprout_parse.mly"
                     ()
# 661 "sprout_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "sprout_parse.mly"
  ()
# 667 "sprout_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 145 "sprout_parse.mly"
             ()
# 674 "sprout_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "sprout_parse.mly"
  ()
# 680 "sprout_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_body) in
    Obj.repr(
# 149 "sprout_parse.mly"
                 ()
# 687 "sprout_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 152 "sprout_parse.mly"
                 ()
# 694 "sprout_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "sprout_parse.mly"
  ()
# 700 "sprout_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "sprout_parse.mly"
       ()
# 706 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "sprout_parse.mly"
        ()
# 712 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "sprout_parse.mly"
      ()
# 718 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "sprout_parse.mly"
      ()
# 724 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "sprout_parse.mly"
     ()
# 730 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "sprout_parse.mly"
      ()
# 736 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "sprout_parse.mly"
     ()
# 742 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "sprout_parse.mly"
     ()
# 748 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "sprout_parse.mly"
      ()
# 754 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "sprout_parse.mly"
      ()
# 760 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "sprout_parse.mly"
      ()
# 766 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "sprout_parse.mly"
     ()
# 772 "sprout_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "sprout_parse.mly"
      ()
# 778 "sprout_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "sprout_parse.mly"
         ()
# 784 "sprout_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 174 "sprout_parse.mly"
           ()
# 791 "sprout_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 175 "sprout_parse.mly"
          ()
# 798 "sprout_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 176 "sprout_parse.mly"
             ()
# 805 "sprout_parse.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sprout_ast.program)
