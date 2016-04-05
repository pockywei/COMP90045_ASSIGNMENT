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

open Parsing;;
let _ = parse_error;;
# 3 "sprout_parse.mly"
open Sprout_ast
# 48 "sprout_parse.ml"
let yytransl_const = [|
  260 (* WRITE *);
  261 (* READ *);
  262 (* ASSIGN *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* EQ *);
  266 (* LT *);
  267 (* GT *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* MUL *);
  271 (* SEMICOLON *);
    0 (* EOF *);
  272 (* COLON *);
  274 (* LEFT_PARENTHESIS *);
  275 (* RIGHT_PARENTHESIS *);
  277 (* TYPEDEF_VALUE_INIT *);
  278 (* DOT *);
  279 (* COMMA *);
  280 (* END *);
  281 (* VAL *);
  282 (* REF *);
  283 (* LEFT_BRACKET *);
  284 (* RIGHT_BRACKET *);
  285 (* WHILE *);
  286 (* DO *);
  287 (* OD *);
  288 (* IF *);
  289 (* THEN *);
  290 (* ELSE *);
  291 (* FI *);
  292 (* BOOL *);
  293 (* INT *);
  294 (* EQ_DOT *);
  295 (* PROC *);
    0|]

let yytransl_block = [|
  257 (* BOOL_VAL *);
  258 (* INT_VAL *);
  259 (* IDENT *);
  273 (* IDENTIFIER *);
  276 (* TYPEDEF *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\006\000\006\000\
\005\000\005\000\005\000\007\000\003\000\003\000\008\000\010\000\
\010\000\012\000\012\000\014\000\014\000\013\000\011\000\011\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\017\000\017\000\016\000\016\000\019\000\019\000\019\000\
\018\000\018\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\006\000\000\000\005\000\004\000\000\000\001\000\000\000\
\001\000\001\000\001\000\003\000\004\000\000\000\005\000\005\000\
\000\000\003\000\000\000\001\000\001\000\001\000\001\000\001\000\
\006\000\004\000\004\000\004\000\005\000\006\000\007\000\003\000\
\000\000\001\000\003\000\000\000\002\000\004\000\005\000\000\000\
\002\000\000\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\054\000\000\000\000\000\000\000\006\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\017\000\043\000\
\044\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\011\000\010\000\000\000\000\000\000\000\002\000\000\000\
\045\000\000\000\000\000\052\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\009\000\006\000\000\000\005\000\023\000\024\000\
\015\000\000\000\027\000\028\000\037\000\000\000\000\000\053\000\
\033\000\033\000\026\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\007\000\004\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\000\000\021\000\
\020\000\018\000\000\000\000\000\025\000\030\000\033\000\000\000\
\016\000\000\000\000\000\035\000\000\000\031\000\000\000\000\000\
\038\000\000\000\039\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\010\000\028\000\076\000\054\000\009\000\
\012\000\032\000\058\000\062\000\079\000\090\000\029\000\039\000\
\083\000\096\000\092\000"

let yysindex = "\007\000\
\000\000\000\000\000\000\245\254\007\255\252\254\000\000\017\255\
\000\000\041\255\037\255\019\255\054\255\057\255\000\000\000\000\
\000\000\119\255\060\255\119\255\251\254\000\000\119\255\119\255\
\119\255\000\000\000\000\063\255\218\255\137\255\000\000\070\255\
\000\000\227\255\056\255\000\000\066\255\000\000\061\255\184\255\
\178\255\115\255\079\255\119\255\119\255\119\255\119\255\119\255\
\119\255\000\000\000\000\000\000\082\255\000\000\000\000\000\000\
\000\000\025\255\000\000\000\000\000\000\087\255\117\255\000\000\
\000\000\000\000\000\000\165\255\165\255\165\255\108\255\108\255\
\000\000\073\255\000\000\000\000\106\255\000\000\029\255\234\255\
\135\255\234\255\127\255\036\255\074\255\000\000\082\255\000\000\
\000\000\000\000\135\255\096\255\000\000\000\000\000\000\125\255\
\000\000\114\255\175\255\000\000\080\255\000\000\082\255\119\255\
\000\000\212\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\199\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\152\255\186\255\187\255\128\255\153\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\255\
\232\255\185\255\000\000\000\000\172\255\000\000\124\255\000\000\
\000\000\000\000\232\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\176\255\000\000\140\255\000\000\
\000\000\140\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\161\000\245\255\219\255\000\000\000\000\
\194\255\000\000\000\000\000\000\000\000\000\000\238\255\000\000\
\000\000\000\000\127\000"

let yytablesize = 296
let yytable = "\034\000\
\014\000\036\000\084\000\085\000\040\000\041\000\042\000\001\000\
\005\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\037\000\009\000\053\000\016\000\017\000\038\000\018\000\019\000\
\007\000\068\000\069\000\070\000\071\000\072\000\073\000\020\000\
\101\000\011\000\008\000\021\000\016\000\017\000\036\000\018\000\
\019\000\051\000\022\000\080\000\082\000\023\000\077\000\024\000\
\020\000\097\000\025\000\088\000\021\000\022\000\026\000\027\000\
\089\000\013\000\022\000\014\000\026\000\027\000\023\000\015\000\
\024\000\105\000\094\000\025\000\107\000\030\000\060\000\026\000\
\027\000\031\000\016\000\017\000\035\000\018\000\019\000\043\000\
\016\000\017\000\061\000\018\000\019\000\106\000\020\000\016\000\
\017\000\013\000\021\000\086\000\020\000\067\000\055\000\056\000\
\021\000\057\000\063\000\020\000\023\000\078\000\024\000\033\000\
\075\000\025\000\023\000\095\000\024\000\026\000\027\000\025\000\
\099\000\023\000\100\000\026\000\027\000\016\000\017\000\016\000\
\017\000\049\000\087\000\044\000\045\000\046\000\047\000\048\000\
\049\000\020\000\099\000\020\000\103\000\033\000\081\000\033\000\
\046\000\046\000\046\000\046\000\046\000\093\000\046\000\023\000\
\046\000\023\000\046\000\066\000\008\000\008\000\046\000\008\000\
\091\000\051\000\052\000\046\000\008\000\046\000\008\000\102\000\
\046\000\047\000\047\000\047\000\047\000\047\000\049\000\047\000\
\049\000\047\000\049\000\047\000\026\000\027\000\049\000\047\000\
\047\000\048\000\049\000\049\000\047\000\049\000\047\000\104\000\
\049\000\047\000\044\000\045\000\046\000\047\000\048\000\049\000\
\044\000\045\000\046\000\047\000\048\000\049\000\001\000\034\000\
\050\000\051\000\050\000\051\000\050\000\051\000\042\000\065\000\
\050\000\051\000\041\000\064\000\074\000\050\000\051\000\050\000\
\051\000\098\000\050\000\051\000\044\000\045\000\046\000\047\000\
\048\000\049\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\000\000\075\000\044\000\045\000\046\000\047\000\048\000\
\049\000\059\000\044\000\045\000\046\000\047\000\048\000\049\000\
\040\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000"

let yycheck = "\018\000\
\000\000\020\000\065\000\066\000\023\000\024\000\025\000\001\000\
\020\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\022\001\017\001\030\000\001\001\002\001\027\001\004\001\005\001\
\018\001\044\000\045\000\046\000\047\000\048\000\049\000\013\001\
\095\000\017\001\039\001\017\001\001\001\002\001\038\001\004\001\
\005\001\017\001\024\001\062\000\063\000\027\001\058\000\029\001\
\013\001\087\000\032\001\023\001\017\001\023\001\036\001\037\001\
\028\001\017\001\028\001\019\001\036\001\037\001\027\001\027\001\
\029\001\103\000\031\001\032\001\106\000\016\001\015\001\036\001\
\037\001\017\001\001\001\002\001\017\001\004\001\005\001\017\001\
\001\001\002\001\017\001\004\001\005\001\104\000\013\001\001\001\
\002\001\017\001\017\001\019\001\013\001\015\001\025\001\026\001\
\017\001\028\001\038\001\013\001\027\001\015\001\029\001\017\001\
\023\001\032\001\027\001\034\001\029\001\036\001\037\001\032\001\
\017\001\027\001\019\001\036\001\037\001\001\001\002\001\001\001\
\002\001\014\001\017\001\009\001\010\001\011\001\012\001\013\001\
\014\001\013\001\017\001\013\001\019\001\017\001\018\001\017\001\
\009\001\010\001\011\001\012\001\013\001\015\001\015\001\027\001\
\017\001\027\001\019\001\033\001\025\001\026\001\023\001\028\001\
\018\001\017\001\018\001\028\001\017\001\030\001\019\001\035\001\
\033\001\009\001\010\001\011\001\012\001\013\001\015\001\015\001\
\017\001\017\001\019\001\019\001\036\001\037\001\023\001\023\001\
\012\001\013\001\014\001\028\001\028\001\030\001\030\001\009\001\
\033\001\033\001\009\001\010\001\011\001\012\001\013\001\014\001\
\009\001\010\001\011\001\012\001\013\001\014\001\000\000\015\001\
\015\001\015\001\017\001\017\001\019\001\019\001\035\001\030\001\
\023\001\023\001\035\001\028\001\052\000\028\001\028\001\030\001\
\030\001\091\000\033\001\033\001\009\001\010\001\011\001\012\001\
\013\001\014\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\023\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\009\001\010\001\011\001\012\001\013\001\014\001\
\017\001\255\255\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\039\001"

let yynames_const = "\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  LT\000\
  GT\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  SEMICOLON\000\
  EOF\000\
  COLON\000\
  LEFT_PARENTHESIS\000\
  RIGHT_PARENTHESIS\000\
  TYPEDEF_VALUE_INIT\000\
  DOT\000\
  COMMA\000\
  END\000\
  VAL\000\
  REF\000\
  LEFT_BRACKET\000\
  RIGHT_BRACKET\000\
  WHILE\000\
  DO\000\
  OD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  BOOL\000\
  INT\000\
  EQ_DOT\000\
  PROC\000\
  "

let yynames_block = "\
  BOOL_VAL\000\
  INT_VAL\000\
  IDENT\000\
  IDENTIFIER\000\
  TYPEDEF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'data_structure) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'function_declaration) in
    Obj.repr(
# 47 "sprout_parse.mly"
                                     ({typedefs = List.rev _1 ; funcdefs = [1]})
# 304 "sprout_parse.ml"
               : Sprout_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'data_structure) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'typedef_body) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "sprout_parse.mly"
                                                                                    ((_4,_6)::_1)
# 314 "sprout_parse.ml"
               : 'data_structure))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "sprout_parse.mly"
  ([])
# 320 "sprout_parse.ml"
               : 'data_structure))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typedef_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_stmts) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'comma_temp) in
    Obj.repr(
# 54 "sprout_parse.mly"
                                                      ( Printf.printf "in yacc %s " _2;SingleTypeTerm((_2,_4))::_1 )
# 330 "sprout_parse.ml"
               : 'typedef_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typedef_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'recursive_list_value_init) in
    Obj.repr(
# 55 "sprout_parse.mly"
                                                          (ListTypeTerm((_2,_4))::_1)
# 339 "sprout_parse.ml"
               : 'typedef_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "sprout_parse.mly"
  ([])
# 345 "sprout_parse.ml"
               : 'typedef_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "sprout_parse.mly"
       ()
# 351 "sprout_parse.ml"
               : 'comma_temp))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "sprout_parse.mly"
 ()
# 357 "sprout_parse.ml"
               : 'comma_temp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "sprout_parse.mly"
            ( IdentType(_1) )
# 364 "sprout_parse.ml"
               : 'type_stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "sprout_parse.mly"
     ( Int )
# 370 "sprout_parse.ml"
               : 'type_stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "sprout_parse.mly"
      ( Bool )
# 376 "sprout_parse.ml"
               : 'type_stmts))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typedef_body) in
    Obj.repr(
# 70 "sprout_parse.mly"
                                                  ( _2 )
# 383 "sprout_parse.ml"
               : 'recursive_list_value_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'function_declaration) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'function_header) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'function_body) in
    Obj.repr(
# 75 "sprout_parse.mly"
                                                        ( (_2,_3)::_1)
# 392 "sprout_parse.ml"
               : 'function_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "sprout_parse.mly"
  ([])
# 398 "sprout_parse.ml"
               : 'function_declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'param_recursive) in
    Obj.repr(
# 80 "sprout_parse.mly"
                                                            ((_2,_4))
# 406 "sprout_parse.ml"
               : 'function_header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'param_recursive) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'val_ref) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'type_stmts) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'comma_temp) in
    Obj.repr(
# 83 "sprout_parse.mly"
                                                           ((_2,_3,_4)::_1)
# 417 "sprout_parse.ml"
               : 'param_recursive))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "sprout_parse.mly"
  ([])
# 423 "sprout_parse.ml"
               : 'param_recursive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argus_recursive) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'argus_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comma_temp_2) in
    Obj.repr(
# 88 "sprout_parse.mly"
                                           ()
# 432 "sprout_parse.ml"
               : 'argus_recursive))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "sprout_parse.mly"
  ()
# 438 "sprout_parse.ml"
               : 'argus_recursive))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "sprout_parse.mly"
               ()
# 444 "sprout_parse.ml"
               : 'comma_temp_2))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "sprout_parse.mly"
       ()
# 450 "sprout_parse.ml"
               : 'comma_temp_2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "sprout_parse.mly"
       ()
# 457 "sprout_parse.ml"
               : 'argus_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "sprout_parse.mly"
     (Val)
# 463 "sprout_parse.ml"
               : 'val_ref))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "sprout_parse.mly"
     (Ref)
# 469 "sprout_parse.ml"
               : 'val_ref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'function_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'dot_term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assign_term) in
    Obj.repr(
# 103 "sprout_parse.mly"
                                                                 ()
# 479 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'function_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_stmts) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "sprout_parse.mly"
                                                ( )
# 488 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'function_body) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "sprout_parse.mly"
                                     (  )
# 496 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'function_body) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 106 "sprout_parse.mly"
                                         (  )
# 504 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'function_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'argus_recursive) in
    Obj.repr(
# 107 "sprout_parse.mly"
                                                                  ()
# 513 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'function_body) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'function_body) in
    Obj.repr(
# 108 "sprout_parse.mly"
                                               ()
# 522 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'function_body) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'function_body) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'else_stmt) in
    Obj.repr(
# 109 "sprout_parse.mly"
                                                         ()
# 532 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'function_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "sprout_parse.mly"
                               ()
# 540 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "sprout_parse.mly"
  ()
# 546 "sprout_parse.ml"
               : 'function_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "sprout_parse.mly"
       ()
# 553 "sprout_parse.ml"
               : 'assign_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_assignment_comma) in
    Obj.repr(
# 115 "sprout_parse.mly"
                                                            ()
# 560 "sprout_parse.ml"
               : 'assign_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "sprout_parse.mly"
 ()
# 566 "sprout_parse.ml"
               : 'dot_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "sprout_parse.mly"
                ()
# 573 "sprout_parse.ml"
               : 'dot_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'value_assignment_comma) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'comma_temp) in
    Obj.repr(
# 123 "sprout_parse.mly"
                                                                      ()
# 581 "sprout_parse.ml"
               : 'value_assignment_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'value_assignment_comma) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'comma_temp) in
    Obj.repr(
# 124 "sprout_parse.mly"
                                                       ()
# 591 "sprout_parse.ml"
               : 'value_assignment_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "sprout_parse.mly"
  ()
# 597 "sprout_parse.ml"
               : 'value_assignment_comma))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'function_body) in
    Obj.repr(
# 129 "sprout_parse.mly"
                     ()
# 604 "sprout_parse.ml"
               : 'else_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "sprout_parse.mly"
  ()
# 610 "sprout_parse.ml"
               : 'else_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 133 "sprout_parse.mly"
             (  )
# 617 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 134 "sprout_parse.mly"
            (  )
# 624 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "sprout_parse.mly"
               (  )
# 631 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "sprout_parse.mly"
                   ( )
# 639 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "sprout_parse.mly"
                    (  )
# 647 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "sprout_parse.mly"
                   (  )
# 655 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "sprout_parse.mly"
                 (  )
# 663 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "sprout_parse.mly"
                 (  )
# 671 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "sprout_parse.mly"
                  (  )
# 679 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "sprout_parse.mly"
                            ( )
# 686 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 143 "sprout_parse.mly"
                                    (  )
# 693 "sprout_parse.ml"
               : 'expr))
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
