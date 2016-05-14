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
  | TYPEDEF
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
# 20 "bean_parse.mly"
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
  276 (* TYPEDEF *);
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
  300 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\005\000\005\000\
\006\000\006\000\003\000\003\000\007\000\010\000\010\000\011\000\
\011\000\012\000\012\000\008\000\008\000\009\000\009\000\013\000\
\013\000\014\000\014\000\014\000\014\000\015\000\015\000\016\000\
\016\000\017\000\017\000\021\000\021\000\022\000\022\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\019\000\019\000\024\000\024\000\020\000\020\000\023\000\023\000\
\023\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\001\000\001\000\003\000\001\000\001\000\
\005\000\003\000\006\000\005\000\004\000\001\000\000\000\005\000\
\003\000\001\000\001\000\004\000\000\000\002\000\001\000\002\000\
\001\000\003\000\002\000\002\000\004\000\006\000\005\000\001\000\
\003\000\001\000\003\000\001\000\000\000\005\000\003\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\001\000\000\000\003\000\001\000\002\000\000\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\066\000\000\000\000\000\000\000\000\000\000\000\
\021\000\007\000\008\000\000\000\005\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\002\000\021\000\018\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\025\000\000\000\000\000\000\000\
\006\000\000\000\013\000\000\000\000\000\063\000\064\000\065\000\
\000\000\000\000\000\000\032\000\000\000\000\000\041\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\022\000\024\000\
\000\000\000\000\010\000\000\000\000\000\000\000\017\000\056\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\026\000\000\000\033\000\
\000\000\011\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\046\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\009\000\
\016\000\031\000\000\000\000\000\000\000\000\000\035\000\000\000\
\000\000\030\000\039\000\000\000\000\000\038\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\033\000\015\000\020\000\009\000\018\000\
\034\000\025\000\026\000\027\000\035\000\036\000\037\000\053\000\
\094\000\095\000\090\000\124\000\118\000\119\000\055\000\091\000"

let yysindex = "\003\000\
\000\000\000\000\000\000\048\255\224\254\255\254\002\255\007\255\
\000\000\000\000\000\000\242\254\000\000\006\255\000\000\224\254\
\060\255\084\255\030\255\037\255\000\000\000\000\000\000\000\000\
\026\255\047\255\255\254\072\255\046\255\072\255\072\255\063\255\
\065\255\097\255\000\000\076\255\000\000\241\254\255\254\071\255\
\000\000\084\255\000\000\060\255\075\255\000\000\000\000\000\000\
\072\255\072\255\072\255\000\000\087\255\126\000\000\000\087\255\
\121\255\092\000\072\255\096\255\000\000\063\255\000\000\000\000\
\043\255\091\255\000\000\104\255\113\255\255\254\000\000\000\000\
\151\000\105\000\072\255\072\255\072\255\072\255\072\255\072\255\
\072\255\072\255\072\255\072\255\072\255\072\255\117\255\117\255\
\126\000\111\255\116\255\000\000\112\255\000\000\126\000\000\000\
\255\254\000\000\114\255\000\000\136\255\136\255\136\255\136\255\
\136\255\136\255\109\255\109\255\000\000\000\000\151\000\139\000\
\127\255\028\255\000\000\072\255\139\255\124\255\129\255\000\000\
\000\000\000\000\117\255\156\255\126\000\043\255\000\000\126\255\
\117\255\000\000\000\000\165\255\043\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\187\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\148\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\150\255\000\000\000\000\000\000\000\000\000\000\227\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\151\255\166\255\000\000\167\255\
\000\000\000\000\152\255\000\000\000\000\004\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\220\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\246\254\000\000\168\255\000\000\169\255\000\000\238\254\000\000\
\000\000\000\000\000\000\000\000\234\255\247\255\005\000\018\000\
\032\000\045\000\174\255\197\255\000\000\000\000\059\000\068\000\
\000\000\201\255\000\000\000\000\000\000\000\000\173\255\000\000\
\000\000\000\000\000\000\000\000\069\255\000\000\000\000\000\000\
\213\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\251\255\000\000\000\000\194\000\205\000\
\217\255\000\000\000\000\188\000\224\255\000\000\000\000\238\255\
\140\255\233\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 437
let yytable = "\038\000\
\014\000\063\000\069\000\001\000\054\000\032\000\057\000\058\000\
\032\000\131\000\056\000\008\000\010\000\011\000\005\000\038\000\
\134\000\016\000\034\000\065\000\034\000\045\000\066\000\038\000\
\034\000\072\000\073\000\074\000\060\000\019\000\060\000\028\000\
\029\000\067\000\030\000\089\000\063\000\031\000\032\000\123\000\
\012\000\032\000\013\000\046\000\047\000\048\000\017\000\113\000\
\114\000\021\000\038\000\101\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\109\000\110\000\111\000\112\000\005\000\
\099\000\039\000\043\000\006\000\038\000\038\000\049\000\062\000\
\046\000\047\000\048\000\040\000\050\000\023\000\024\000\041\000\
\063\000\063\000\051\000\129\000\093\000\044\000\052\000\028\000\
\029\000\052\000\030\000\120\000\125\000\031\000\038\000\038\000\
\063\000\010\000\011\000\049\000\028\000\029\000\059\000\030\000\
\038\000\050\000\031\000\059\000\060\000\059\000\038\000\051\000\
\064\000\061\000\068\000\052\000\028\000\029\000\071\000\030\000\
\028\000\029\000\031\000\030\000\066\000\012\000\031\000\032\000\
\087\000\098\000\028\000\029\000\092\000\030\000\096\000\122\000\
\031\000\083\000\084\000\097\000\062\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\115\000\
\085\000\086\000\116\000\117\000\062\000\121\000\040\000\126\000\
\062\000\040\000\081\000\082\000\083\000\084\000\127\000\128\000\
\130\000\132\000\062\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\043\000\040\000\040\000\
\043\000\133\000\001\000\040\000\015\000\040\000\014\000\040\000\
\058\000\040\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\028\000\027\000\044\000\043\000\043\000\044\000\
\057\000\022\000\043\000\037\000\043\000\062\000\043\000\036\000\
\043\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\061\000\042\000\055\000\044\000\044\000\055\000\070\000\
\000\000\044\000\000\000\044\000\000\000\044\000\000\000\044\000\
\000\000\047\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\055\000\000\000\048\000\000\000\
\055\000\048\000\055\000\000\000\055\000\000\000\055\000\000\000\
\000\000\047\000\047\000\000\000\049\000\000\000\047\000\049\000\
\047\000\000\000\047\000\000\000\047\000\000\000\048\000\048\000\
\000\000\051\000\000\000\048\000\051\000\048\000\000\000\048\000\
\000\000\048\000\000\000\000\000\049\000\049\000\000\000\050\000\
\000\000\049\000\050\000\049\000\000\000\049\000\000\000\049\000\
\000\000\051\000\051\000\000\000\052\000\000\000\051\000\052\000\
\051\000\000\000\051\000\000\000\051\000\000\000\000\000\050\000\
\050\000\000\000\053\000\000\000\050\000\053\000\050\000\000\000\
\050\000\000\000\050\000\054\000\052\000\052\000\054\000\000\000\
\000\000\052\000\000\000\052\000\000\000\052\000\000\000\052\000\
\000\000\000\000\053\000\053\000\000\000\000\000\000\000\053\000\
\000\000\053\000\000\000\053\000\054\000\053\000\088\000\000\000\
\054\000\000\000\054\000\000\000\054\000\000\000\054\000\000\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\084\000\000\000\085\000\086\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\000\000\
\085\000\086\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\100\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\000\000\085\000\086\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\000\000\085\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000"

let yycheck = "\018\000\
\006\000\034\000\042\000\001\000\028\000\035\001\030\000\031\000\
\038\001\126\000\029\000\044\001\014\001\015\001\044\001\034\000\
\133\000\016\001\037\001\035\001\039\001\027\000\038\001\042\000\
\043\001\049\000\050\000\051\000\039\001\044\001\041\001\004\001\
\005\001\039\000\007\001\059\000\069\000\010\001\035\001\012\001\
\042\001\038\001\044\001\001\001\002\001\003\001\040\001\087\000\
\088\000\044\001\069\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\016\001\
\070\000\036\001\041\001\020\001\087\000\088\000\028\001\044\001\
\001\001\002\001\003\001\039\001\034\001\018\001\019\001\043\001\
\113\000\114\000\040\001\123\000\042\001\039\001\044\001\004\001\
\005\001\044\001\007\001\097\000\116\000\010\001\113\000\114\000\
\129\000\014\001\015\001\028\001\004\001\005\001\040\001\007\001\
\123\000\034\001\010\001\039\001\044\001\041\001\129\000\040\001\
\037\001\017\001\044\001\044\001\004\001\005\001\044\001\007\001\
\004\001\005\001\010\001\007\001\038\001\042\001\010\001\044\001\
\008\001\017\001\004\001\005\001\037\001\007\001\044\001\009\001\
\010\001\029\001\030\001\036\001\044\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\041\001\
\032\001\033\001\039\001\044\001\044\001\044\001\008\001\021\001\
\044\001\011\001\027\001\028\001\029\001\030\001\043\001\039\001\
\013\001\044\001\044\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\008\001\032\001\033\001\
\011\001\021\001\000\000\037\001\041\001\039\001\041\001\041\001\
\041\001\043\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\037\001\037\001\008\001\032\001\033\001\011\001\
\041\001\016\000\037\001\043\001\039\001\013\001\041\001\043\001\
\043\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\013\001\022\000\008\001\032\001\033\001\011\001\044\000\
\255\255\037\001\255\255\039\001\255\255\041\001\255\255\043\001\
\255\255\008\001\255\255\255\255\011\001\255\255\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\255\255\008\001\255\255\
\037\001\011\001\039\001\255\255\041\001\255\255\043\001\255\255\
\255\255\032\001\033\001\255\255\008\001\255\255\037\001\011\001\
\039\001\255\255\041\001\255\255\043\001\255\255\032\001\033\001\
\255\255\008\001\255\255\037\001\011\001\039\001\255\255\041\001\
\255\255\043\001\255\255\255\255\032\001\033\001\255\255\008\001\
\255\255\037\001\011\001\039\001\255\255\041\001\255\255\043\001\
\255\255\032\001\033\001\255\255\008\001\255\255\037\001\011\001\
\039\001\255\255\041\001\255\255\043\001\255\255\255\255\032\001\
\033\001\255\255\008\001\255\255\037\001\011\001\039\001\255\255\
\041\001\255\255\043\001\008\001\032\001\033\001\011\001\255\255\
\255\255\037\001\255\255\039\001\255\255\041\001\255\255\043\001\
\255\255\255\255\032\001\033\001\255\255\255\255\255\255\037\001\
\255\255\039\001\255\255\041\001\033\001\043\001\011\001\255\255\
\037\001\255\255\039\001\255\255\041\001\255\255\043\001\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\255\255\
\032\001\033\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\041\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\255\255\032\001\033\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001"

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
  TYPEDEF\000\
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
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procedure_definition) in
    Obj.repr(
# 71 "bean_parse.mly"
                                       ({typedefs = List.rev _1;funcdefs = List.rev _2})
# 368 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "bean_parse.mly"
                                               ((_3,_4)::_1)
# 377 "bean_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "bean_parse.mly"
  ([])
# 383 "bean_parse.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive_type) in
    Obj.repr(
# 79 "bean_parse.mly"
                 (_1)
# 390 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "bean_parse.mly"
             (SingleTypeTerm((IdentType _1)))
# 397 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_definition) in
    Obj.repr(
# 81 "bean_parse.mly"
                                          (ListTypeTerm(List.rev _2))
# 404 "bean_parse.ml"
               : 'type_spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "bean_parse.mly"
       (SingleTypeTerm(Bool))
# 410 "bean_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "bean_parse.mly"
      (SingleTypeTerm(Int))
# 416 "bean_parse.ml"
               : 'primitive_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'field_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'type_spec) in
    Obj.repr(
# 89 "bean_parse.mly"
                                                    (SingleTypeTermWithIdent(_3,_5)::_1)
# 425 "bean_parse.ml"
               : 'field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_spec) in
    Obj.repr(
# 90 "bean_parse.mly"
                             (SingleTypeTermWithIdent(_1,_3)::[])
# 433 "bean_parse.ml"
               : 'field_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'procedure_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'procedure_header) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_definition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 94 "bean_parse.mly"
                                                                               ((_3,List.rev _4,List.rev _5)::_1)
# 443 "bean_parse.ml"
               : 'procedure_definition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'procedure_header) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'variable_definition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 95 "bean_parse.mly"
                                                          ((_2,List.rev _3,List.rev _4)::[])
# 452 "bean_parse.ml"
               : 'procedure_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 98 "bean_parse.mly"
                                           ((_1,List.rev _3))
# 460 "bean_parse.ml"
               : 'procedure_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 102 "bean_parse.mly"
        (_1)
# 467 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "bean_parse.mly"
  ([])
# 473 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'pass_type) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "bean_parse.mly"
                                             ((_3,_4,_5)::_1)
# 483 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pass_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "bean_parse.mly"
                                 ((_1,_2,_3)::[])
# 492 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "bean_parse.mly"
      (Val)
# 498 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "bean_parse.mly"
      (Ref)
# 504 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 116 "bean_parse.mly"
                                                     ( SingleTypeTermWithIdent(_3,_2)::_1 )
# 513 "bean_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "bean_parse.mly"
  ([])
# 519 "bean_parse.ml"
               : 'variable_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "bean_parse.mly"
                 (_2::_1)
# 527 "bean_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 122 "bean_parse.mly"
       (_1::[])
# 534 "bean_parse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_stmt) in
    Obj.repr(
# 125 "bean_parse.mly"
                        (_1)
# 541 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_stmt) in
    Obj.repr(
# 126 "bean_parse.mly"
                (_1)
# 548 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 129 "bean_parse.mly"
                       (Assign(_1,_3))
# 556 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 130 "bean_parse.mly"
              (Read(_2))
# 563 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "bean_parse.mly"
             (Write(_2))
# 570 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 132 "bean_parse.mly"
                                              (Method(_1,List.rev _3))
# 578 "bean_parse.ml"
               : 'atomic_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_block) in
    Obj.repr(
# 135 "bean_parse.mly"
                                       (IfDec(_2,List.rev _4,_5))
# 587 "bean_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 136 "bean_parse.mly"
                             (WhileDec(_2,List.rev _4))
# 595 "bean_parse.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "bean_parse.mly"
             (LId(_1))
# 602 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 140 "bean_parse.mly"
                        (LField(_1,_3))
# 610 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "bean_parse.mly"
       ( Rexpr(_1) )
# 617 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_inits) in
    Obj.repr(
# 144 "bean_parse.mly"
                                     (Rstmts(List.rev _2))
# 624 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field_init) in
    Obj.repr(
# 148 "bean_parse.mly"
             (_1)
# 631 "bean_parse.ml"
               : 'field_inits))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "bean_parse.mly"
  ([])
# 637 "bean_parse.ml"
               : 'field_inits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'field_init) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 152 "bean_parse.mly"
                                        (Rassign(_3,_5)::_1)
# 646 "bean_parse.ml"
               : 'field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 153 "bean_parse.mly"
                       (Rassign(_1,_3)::[])
# 654 "bean_parse.ml"
               : 'field_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 156 "bean_parse.mly"
         ( Elval(_1) )
# 661 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 157 "bean_parse.mly"
        ( _1 )
# 668 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "bean_parse.mly"
                              (Ebracket(_2))
# 675 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "bean_parse.mly"
                 (Ebinop(_1,Op_add,_3))
# 683 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "bean_parse.mly"
                  (Ebinop(_1,Op_sub,_3))
# 691 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "bean_parse.mly"
                (Ebinop(_1,Op_mul,_3))
# 699 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "bean_parse.mly"
                (Ebinop(_1,Op_div,_3))
# 707 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "bean_parse.mly"
               (Ebinop(_1,Op_eq,_3))
# 715 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "bean_parse.mly"
                (Ebinop(_1,Op_neq,_3))
# 723 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "bean_parse.mly"
               (Ebinop(_1,Op_lt,_3))
# 731 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "bean_parse.mly"
               (Ebinop(_1,Op_gt,_3))
# 739 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "bean_parse.mly"
                (Ebinop(_1,Op_lte,_3))
# 747 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "bean_parse.mly"
                (Ebinop(_1,Op_gte,_3))
# 755 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "bean_parse.mly"
                (Ebinop(_1,Op_and,_3))
# 763 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "bean_parse.mly"
               (Ebinop(_1,Op_or,_3))
# 771 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 171 "bean_parse.mly"
           (Eunop(Op_not,_2))
# 778 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "bean_parse.mly"
                          (Eunop(Op_minus,_2))
# 785 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 175 "bean_parse.mly"
        (_1)
# 792 "bean_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "bean_parse.mly"
  ([])
# 798 "bean_parse.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "bean_parse.mly"
                   (_3::_1)
# 806 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "bean_parse.mly"
       (_1::[])
# 813 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 183 "bean_parse.mly"
                 (List.rev _2)
# 820 "bean_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "bean_parse.mly"
  ([])
# 826 "bean_parse.ml"
               : 'else_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 187 "bean_parse.mly"
           (Ebool(_1))
# 833 "bean_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 188 "bean_parse.mly"
          (Eint(_1))
# 840 "bean_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 189 "bean_parse.mly"
             (Eident(_1))
# 847 "bean_parse.ml"
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
