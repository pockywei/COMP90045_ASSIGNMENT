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
\011\000\016\000\016\000\015\000\015\000\015\000\015\000\017\000\
\017\000\018\000\018\000\019\000\019\000\019\000\024\000\025\000\
\025\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\021\000\021\000\027\000\027\000\022\000\023\000\
\023\000\026\000\026\000\026\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\001\000\001\000\003\000\001\000\001\000\
\004\000\002\000\000\000\006\000\001\000\000\000\004\000\004\000\
\000\000\002\000\000\000\001\000\001\000\004\000\000\000\003\000\
\002\000\001\000\000\000\003\000\002\000\002\000\004\000\006\000\
\005\000\001\000\003\000\001\000\003\000\002\000\004\000\002\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000\000\000\002\000\000\000\001\000\002\000\
\000\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\069\000\000\000\000\000\000\000\000\000\005\000\
\011\000\007\000\008\000\000\000\004\000\000\000\000\000\000\000\
\002\000\000\000\023\000\010\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\015\000\018\000\
\020\000\021\000\000\000\066\000\067\000\068\000\000\000\000\000\
\034\000\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\026\000\025\000\000\000\
\000\000\000\000\058\000\057\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\024\000\035\000\
\000\000\028\000\000\000\016\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\048\000\000\000\
\000\000\000\000\059\000\031\000\063\000\000\000\000\000\038\000\
\000\000\000\000\061\000\033\000\000\000\000\000\040\000\037\000\
\000\000\064\000\032\000\000\000\039\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\012\000\013\000\015\000\016\000\007\000\
\019\000\024\000\109\000\026\000\027\000\043\000\035\000\063\000\
\036\000\051\000\090\000\082\000\083\000\110\000\118\000\113\000\
\114\000\053\000\107\000"

let yysindex = "\017\000\
\000\000\000\000\000\000\245\254\239\254\000\000\233\254\000\000\
\000\000\000\000\000\000\005\255\000\000\016\255\042\255\056\255\
\000\000\057\255\000\000\000\000\000\000\068\255\000\000\031\255\
\239\254\043\255\081\255\010\255\066\255\084\255\010\255\010\255\
\098\255\095\255\119\255\052\255\033\255\000\000\000\000\000\000\
\000\000\000\000\239\254\000\000\000\000\000\000\010\255\010\255\
\000\000\010\255\111\255\123\000\000\000\111\255\010\255\204\255\
\189\255\121\255\000\000\052\255\084\255\000\000\000\000\118\255\
\007\255\122\255\000\000\000\000\104\000\010\255\010\255\010\255\
\010\255\010\255\010\255\010\255\010\255\010\255\010\255\010\255\
\010\255\222\255\124\255\052\255\052\255\000\000\000\000\000\000\
\113\255\000\000\123\000\000\000\000\000\078\255\078\255\078\255\
\078\255\078\255\078\255\112\255\112\255\000\000\000\000\152\000\
\138\000\010\255\000\000\000\000\000\000\109\255\110\255\000\000\
\054\255\128\255\000\000\000\000\052\255\114\255\000\000\000\000\
\146\255\000\000\000\000\007\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\233\254\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\091\255\000\000\
\000\000\000\000\000\000\000\000\000\000\037\255\000\000\000\000\
\000\000\000\000\000\000\077\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\090\255\135\255\000\000\137\255\133\255\000\000\
\000\000\000\000\000\000\077\255\039\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\136\255\000\000\000\000\000\000\000\000\000\000\000\000\
\148\255\000\000\051\255\000\000\000\000\225\255\246\255\002\000\
\014\000\035\000\047\000\123\255\156\255\000\000\000\000\059\000\
\071\000\133\255\000\000\000\000\000\000\000\000\131\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\237\255\000\000\000\000\000\000\000\000\100\000\
\000\000\234\255\049\000\228\255\068\000\194\255\000\000\000\000\
\000\000\000\000\000\000"

let yytablesize = 424
let yytable = "\052\000\
\001\000\037\000\056\000\057\000\034\000\008\000\054\000\044\000\
\045\000\046\000\044\000\045\000\046\000\037\000\005\000\009\000\
\062\000\001\000\067\000\068\000\014\000\069\000\111\000\047\000\
\010\000\011\000\047\000\017\000\048\000\049\000\050\000\048\000\
\049\000\050\000\028\000\029\000\091\000\037\000\018\000\089\000\
\062\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\030\000\122\000\028\000\
\029\000\033\000\038\000\005\000\064\000\037\000\037\000\009\000\
\034\000\031\000\034\000\039\000\032\000\036\000\020\000\040\000\
\010\000\011\000\061\000\021\000\066\000\065\000\022\000\036\000\
\023\000\034\000\119\000\034\000\036\000\025\000\031\000\120\000\
\049\000\032\000\076\000\077\000\078\000\079\000\037\000\091\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\027\000\055\000\042\000\042\000\042\000\041\000\
\042\000\027\000\042\000\017\000\027\000\027\000\042\000\017\000\
\058\000\019\000\019\000\042\000\059\000\042\000\078\000\079\000\
\042\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\060\000\064\000\086\000\088\000\045\000\045\000\045\000\
\092\000\116\000\112\000\045\000\108\000\117\000\121\000\045\000\
\124\000\030\000\123\000\029\000\045\000\060\000\045\000\087\000\
\062\000\045\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\041\000\065\000\125\000\115\000\046\000\046\000\
\046\000\000\000\000\000\000\000\046\000\000\000\000\000\000\000\
\046\000\000\000\000\000\000\000\000\000\046\000\000\000\046\000\
\000\000\000\000\046\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\000\000\000\000\000\000\
\080\000\081\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\000\000\000\000\000\000\080\000\
\081\000\000\000\000\000\085\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\000\000\084\000\
\000\000\080\000\081\000\049\000\049\000\049\000\000\000\000\000\
\000\000\049\000\106\000\000\000\000\000\049\000\000\000\000\000\
\000\000\000\000\049\000\000\000\049\000\000\000\000\000\049\000\
\050\000\050\000\050\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\050\000\000\000\051\000\051\000\051\000\050\000\
\000\000\050\000\051\000\000\000\050\000\000\000\051\000\000\000\
\053\000\053\000\053\000\051\000\000\000\051\000\053\000\000\000\
\051\000\000\000\053\000\000\000\013\000\000\000\000\000\053\000\
\000\000\053\000\000\000\000\000\053\000\052\000\052\000\052\000\
\000\000\000\000\000\000\052\000\000\000\000\000\000\000\052\000\
\000\000\054\000\054\000\054\000\052\000\000\000\052\000\054\000\
\000\000\052\000\000\000\054\000\000\000\055\000\055\000\055\000\
\054\000\000\000\054\000\055\000\000\000\054\000\000\000\055\000\
\000\000\056\000\000\000\056\000\055\000\000\000\055\000\056\000\
\000\000\055\000\000\000\056\000\000\000\000\000\000\000\000\000\
\056\000\000\000\056\000\000\000\000\000\056\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\000\000\000\000\000\000\080\000\081\000\000\000\000\000\000\000\
\093\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\000\000\000\000\000\000\080\000\081\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\000\000\000\000\000\000\080\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000"

let yycheck = "\028\000\
\000\000\024\000\031\000\032\000\024\000\023\001\029\000\001\001\
\002\001\003\001\001\001\002\001\003\001\036\000\026\001\033\001\
\036\000\001\000\047\000\048\000\044\001\050\000\085\000\017\001\
\042\001\043\001\017\001\023\001\022\001\023\001\024\001\022\001\
\023\001\024\001\004\001\005\001\065\000\060\000\023\001\033\001\
\060\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\023\001\117\000\004\001\
\005\001\024\000\025\000\023\001\028\001\084\000\085\000\033\001\
\028\001\035\001\028\001\025\001\038\001\019\001\029\001\029\001\
\042\001\043\001\023\001\034\001\043\000\045\001\023\001\029\001\
\024\001\045\001\029\001\045\001\034\001\018\001\035\001\034\001\
\023\001\038\001\013\001\014\001\015\001\016\001\117\000\124\000\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\030\001\024\001\019\001\020\001\021\001\031\001\
\032\001\037\001\025\001\025\001\040\001\041\001\029\001\029\001\
\023\001\031\001\032\001\034\001\030\001\036\001\015\001\016\001\
\039\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\019\001\028\001\019\001\023\001\019\001\020\001\021\001\
\023\001\037\001\034\001\025\001\025\001\040\001\023\001\029\001\
\007\001\019\001\041\001\019\001\034\001\025\001\036\001\060\000\
\025\001\039\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\023\001\041\001\124\000\106\000\019\001\020\001\
\021\001\255\255\255\255\255\255\025\001\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\255\255\036\001\
\255\255\255\255\039\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\255\255\
\020\001\021\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\255\255\255\255\020\001\
\021\001\255\255\255\255\039\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\036\001\
\255\255\020\001\021\001\019\001\020\001\021\001\255\255\255\255\
\255\255\025\001\029\001\255\255\255\255\029\001\255\255\255\255\
\255\255\255\255\034\001\255\255\036\001\255\255\255\255\039\001\
\019\001\020\001\021\001\255\255\255\255\255\255\025\001\255\255\
\255\255\255\255\029\001\255\255\019\001\020\001\021\001\034\001\
\255\255\036\001\025\001\255\255\039\001\255\255\029\001\255\255\
\019\001\020\001\021\001\034\001\255\255\036\001\025\001\255\255\
\039\001\255\255\029\001\255\255\044\001\255\255\255\255\034\001\
\255\255\036\001\255\255\255\255\039\001\019\001\020\001\021\001\
\255\255\255\255\255\255\025\001\255\255\255\255\255\255\029\001\
\255\255\019\001\020\001\021\001\034\001\255\255\036\001\025\001\
\255\255\039\001\255\255\029\001\255\255\019\001\020\001\021\001\
\034\001\255\255\036\001\025\001\255\255\039\001\255\255\029\001\
\255\255\019\001\255\255\021\001\034\001\255\255\036\001\025\001\
\255\255\039\001\255\255\029\001\255\255\255\255\255\255\255\255\
\034\001\255\255\036\001\255\255\255\255\039\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\020\001\021\001\255\255\255\255\255\255\
\025\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\020\001\021\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\020\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001"

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
# 57 "sprout_parse.mly"
                                       ({typedefs = List.rev _1;funcdefs = List.rev _2})
# 366 "sprout_parse.ml"
               : Sprout_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "sprout_parse.mly"
                                               ((_3,_4)::_1)
# 376 "sprout_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "sprout_parse.mly"
  ([])
# 382 "sprout_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive_type) in
    Obj.repr(
# 65 "sprout_parse.mly"
                 (_1)
# 389 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "sprout_parse.mly"
             (SingleTypeTerm((IdentType _1)))
# 396 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 68 "sprout_parse.mly"
                                          (ListTypeTerm( List.rev _2))
# 403 "sprout_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "sprout_parse.mly"
       (SingleTypeTerm(Bool))
# 409 "sprout_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "sprout_parse.mly"
      (SingleTypeTerm(Int))
# 415 "sprout_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_spec) in
    Obj.repr(
# 75 "sprout_parse.mly"
                                                  (SingleTypeTermWithIdent(_2,_4)::_1)
# 424 "sprout_parse.ml"
               : 'field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 78 "sprout_parse.mly"
                         (_1)
# 431 "sprout_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "sprout_parse.mly"
  ([])
# 437 "sprout_parse.ml"
               : 'rec_field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rec_procedure_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'procedure_header) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_definition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'procedure_body) in
    Obj.repr(
# 91 "sprout_parse.mly"
                                                                                        ((_3,_4,_5)::_1)
# 447 "sprout_parse.ml"
               : 'procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 95 "sprout_parse.mly"
                       (_1)
# 454 "sprout_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "sprout_parse.mly"
  ([])
# 460 "sprout_parse.ml"
               : 'rec_procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 101 "sprout_parse.mly"
                                          ((_1,_3))
# 468 "sprout_parse.ml"
               : 'procedure_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_param) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'param_passing_indicator) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "sprout_parse.mly"
                                                         ((_2,_3,_4)::_1)
# 478 "sprout_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "sprout_parse.mly"
  ([])
# 484 "sprout_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 112 "sprout_parse.mly"
              (_1)
# 491 "sprout_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "sprout_parse.mly"
  ([])
# 497 "sprout_parse.ml"
               : 'rec_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "sprout_parse.mly"
      (Val)
# 503 "sprout_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "sprout_parse.mly"
      (Ref)
# 509 "sprout_parse.ml"
               : 'param_passing_indicator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "sprout_parse.mly"
                                                     ( SingleTypeTermWithIdent(_3,_2)::_1 )
# 518 "sprout_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "sprout_parse.mly"
  ([])
# 524 "sprout_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rec_procedure_body) in
    Obj.repr(
# 130 "sprout_parse.mly"
                                           (_1::_3)
# 532 "sprout_parse.ml"
               : 'procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'compound_stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rec_procedure_body) in
    Obj.repr(
# 131 "sprout_parse.mly"
                                   (_1::_2)
# 540 "sprout_parse.ml"
               : 'procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_body) in
    Obj.repr(
# 134 "sprout_parse.mly"
                 (_1)
# 547 "sprout_parse.ml"
               : 'rec_procedure_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "sprout_parse.mly"
  ([])
# 553 "sprout_parse.ml"
               : 'rec_procedure_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 139 "sprout_parse.mly"
                       ( Assign(_1,_3))
# 561 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 140 "sprout_parse.mly"
              ( Read(_2) )
# 568 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "sprout_parse.mly"
             ( Write(_2) )
# 575 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 142 "sprout_parse.mly"
                                              ( Method(_1,_3) )
# 583 "sprout_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_block) in
    Obj.repr(
# 146 "sprout_parse.mly"
                                       (IfDec(_2,_4,_5))
# 592 "sprout_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 147 "sprout_parse.mly"
                             (WhileDec(_2,_4))
# 600 "sprout_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "sprout_parse.mly"
             ( LId(_1) )
# 607 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "sprout_parse.mly"
                        ( LField(_1,_3) )
# 615 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "sprout_parse.mly"
       ( Rexpr(_1) )
# 622 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 158 "sprout_parse.mly"
                                    ( Rstmts(_2) )
# 629 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "sprout_parse.mly"
                         ( Rempty )
# 635 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rec_field_init) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 163 "sprout_parse.mly"
                                      (Rassign(_2,_4)::_1)
# 644 "sprout_parse.ml"
               : 'field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_init) in
    Obj.repr(
# 166 "sprout_parse.mly"
                   (_1)
# 651 "sprout_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "sprout_parse.mly"
  ([])
# 657 "sprout_parse.ml"
               : 'rec_field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 170 "sprout_parse.mly"
         ( Elval(_1) )
# 664 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 171 "sprout_parse.mly"
        ( _1 )
# 671 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 172 "sprout_parse.mly"
                              ( Ebracket(_2) )
# 678 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "sprout_parse.mly"
                 ( Ebinop(_1,Op_add,_3) )
# 686 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "sprout_parse.mly"
                  ( Ebinop(_1,Op_sub,_3) )
# 694 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "sprout_parse.mly"
                ( Ebinop(_1,Op_mul,_3) )
# 702 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "sprout_parse.mly"
                ( Ebinop(_1,Op_div,_3) )
# 710 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "sprout_parse.mly"
               ( Ebinop(_1,Op_eq,_3) )
# 718 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "sprout_parse.mly"
                ( Ebinop(_1,Op_neq,_3) )
# 726 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "sprout_parse.mly"
               ( Ebinop(_1,Op_lt,_3) )
# 734 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "sprout_parse.mly"
               ( Ebinop(_1,Op_gt,_3) )
# 742 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "sprout_parse.mly"
                ( Ebinop(_1,Op_lte,_3) )
# 750 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "sprout_parse.mly"
                ( Ebinop(_1,Op_gte,_3) )
# 758 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "sprout_parse.mly"
                ( Ebinop(_1,Op_and,_3) )
# 766 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "sprout_parse.mly"
               ( Ebinop(_1,Op_or,_3) )
# 774 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "sprout_parse.mly"
           ( Eunop(Op_not,_2) )
# 781 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "sprout_parse.mly"
              ( Eunop(Op_minus,_2) )
# 788 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rec_expr_list) in
    Obj.repr(
# 190 "sprout_parse.mly"
                     ( _1::_2 )
# 796 "sprout_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "sprout_parse.mly"
  ([])
# 802 "sprout_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 194 "sprout_parse.mly"
                  ( _2 )
# 809 "sprout_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "sprout_parse.mly"
  ([])
# 815 "sprout_parse.ml"
               : 'rec_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_body) in
    Obj.repr(
# 198 "sprout_parse.mly"
                 (_1)
# 822 "sprout_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 201 "sprout_parse.mly"
                 (_2)
# 829 "sprout_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "sprout_parse.mly"
  ([])
# 835 "sprout_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 205 "sprout_parse.mly"
           ( Ebool(_1) )
# 842 "sprout_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 206 "sprout_parse.mly"
          ( Eint(_1) )
# 849 "sprout_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 207 "sprout_parse.mly"
             ( Eident(_1) )
# 856 "sprout_parse.ml"
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
