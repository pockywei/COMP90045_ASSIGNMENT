/*
 * The is the file contains our yacc or context free grammar to build the syntax
 * tree in order to check input file meets the syntax of the bean language.
 *
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
 *
*/

%{
open Bean_ast
%}

/* Constants */
%token <bool> BOOL_VAL
%token <int> INT_VAL
%token <string> STRING_VAL
/* Keywords */
%token WRITE READ
%token ASSIGN
%token WHILE DO OD
%token IF THEN ELSE FI
%token BOOL INT
%token PROC
%token END
%token VAL
%token REF
%token <string> TYPEDEF
/* Operators */
%token EQ NEQ LT LTE GT GTE
%token PLUS MINUS MUL DIV
%token UMINUS
%token AND OR NOT
%token EQ_COL
/* Punctuation */
%token COLON
%token SEMICOLON
%token DOT
%token COMMA
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACE RIGHT_BRACE
/* Miscellaneous */
%token EOF
%token <string> IDENTIFIER

/* Precedence */
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left MUL DIV
%right EQ_COL
%nonassoc UMINUS

%type <Bean_ast.program> start_state

%start start_state
%%

start_state:
| type_definition procedure_definition {{typedefs = List.rev $1;funcdefs = List.rev $2}}

/* Zero or more type definitions */
type_definition:
| type_definition TYPEDEF type_spec IDENTIFIER {($3,$4)::$1}
| {[]}

type_spec:
| primitive_type {$1}
| IDENTIFIER {SingleTypeTerm((IdentType $1))}
| LEFT_BRACE field_definition RIGHT_BRACE {ListTypeTerm( List.rev $2)}

primitive_type:
| BOOL {SingleTypeTerm(Bool)}
| INT {SingleTypeTerm(Int)}

/* One or more field definitions */
field_definition:
| field_definition COMMA IDENTIFIER COLON type_spec {SingleTypeTermWithIdent($3,$5)::$1}
| IDENTIFIER COLON type_spec {SingleTypeTermWithIdent($1,$3)::[]}

/* One or more procedures */
procedure_definition:
| procedure_definition PROC procedure_header variable_definition stmt_list END {($3,List.rev $4,List.rev $5)::$1}
| PROC procedure_header variable_definition stmt_list END {($2,List.rev $3,List.rev $4)::[]}

procedure_header:
| IDENTIFIER LEFT_PAREN params RIGHT_PAREN {($1,List.rev $3)}

/* Zero or more parameters */
params:
| param {$1}
| {[]}

param:
| param COMMA pass_type type_spec IDENTIFIER {($3,$4,$5)::$1}
| pass_type type_spec IDENTIFIER {($1,$2,$3)::[]}

/* Pass by value or reference */
pass_type:
| VAL {Val}
| REF {Ref}

/* Zero or more local variables */
variable_definition:
| variable_definition type_spec IDENTIFIER SEMICOLON { SingleTypeTermWithIdent($3,$2)::$1 }
| {[]}

/* One or more statements */
stmt_list:
| stmt_list stmt {$2::$1}
| stmt {$1::[]}

stmt:
| atomic_stmt SEMICOLON {$1}
| compound_stmt {$1}

atomic_stmt:
| lvalue EQ_COL rvalue { Assign($1,$3)}
| READ lvalue { Read($2) }
| WRITE expr { Write($2) }
| IDENTIFIER LEFT_PAREN expr_list RIGHT_PAREN { Method($1,List.rev $3) }

compound_stmt:
| IF expr THEN stmt_list else_block FI {IfDec($2,List.rev $4,$5)}
| WHILE expr DO stmt_list OD {WhileDec($2,List.rev $4)}

lvalue:
| IDENTIFIER { LId($1) }
| lvalue DOT IDENTIFIER { LField($1,$3) }

rvalue:
| expr { Rexpr($1) }
| LEFT_BRACE field_inits RIGHT_BRACE { Rstmts(List.rev $2) }
/* TODO: Removed REmpty rule, deal with in AST */

field_inits:
| field_init {$1}
| {[]}

field_init:
| field_init COMMA IDENTIFIER EQ rvalue {Rassign($3,$5)::$1}
| IDENTIFIER EQ rvalue {Rassign($1,$3)::[]}

expr:
| lvalue { Elval($1) }
| const { $1 }
| LEFT_PAREN expr RIGHT_PAREN { Ebracket($2) }
| expr PLUS expr { Ebinop($1,Op_add,$3) }
| expr MINUS expr { Ebinop($1,Op_sub,$3) }
| expr MUL expr { Ebinop($1,Op_mul,$3) }
| expr DIV expr { Ebinop($1,Op_div,$3) }
| expr EQ expr { Ebinop($1,Op_eq,$3) }
| expr NEQ expr { Ebinop($1,Op_neq,$3) }
| expr LT expr { Ebinop($1,Op_lt,$3) }
| expr GT expr { Ebinop($1,Op_gt,$3) }
| expr LTE expr { Ebinop($1,Op_lte,$3) }
| expr GTE expr { Ebinop($1,Op_gte,$3) }
| expr AND expr { Ebinop($1,Op_and,$3) }
| expr OR expr { Ebinop($1,Op_or,$3) }
| NOT expr { Eunop(Op_not,$2) }
| MINUS expr %prec UMINUS { Eunop(Op_minus,$2) } /* Precedence for unary minus */

expr_list:
| exprs {$1}
| {[]}

exprs:
| exprs COMMA expr { $3::$1 }
| expr { $1::[] }

else_block:
| ELSE stmt_list {List.rev $2}
| {[]}

const:
| BOOL_VAL { Ebool($1) }
| INT_VAL { Eint($1) }
| STRING_VAL { Eident($1) }
