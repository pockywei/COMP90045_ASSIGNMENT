/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool> BOOL_VAL
%token <int> INT_VAL
%token <string> STRING_VAL
%token WRITE READ
%token ASSIGN
%token EQ NEQ LT LTE GT GTE
%token PLUS MINUS MUL DIV
%token UMINUS
%token COLON
%token SEMICOLON
%token AND OR NOT
%token EOF


%token <string> IDENTIFIER
%token LEFT_PAREN RIGHT_PAREN
%token <string> TYPEDEF
%token TYPEDEF_VALUE_INIT
%token DOT
%token COMMA
%token END
%token VAL
%token REF
%token LEFT_BRACE RIGHT_BRACE
%token WHILE DO OD
%token IF THEN ELSE FI
%token BOOL INT
%token PROC
%token EQ_COL
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left MUL DIV
%right EQ_COL
%nonassoc UMINUS

%type <Sprout_ast.program> start_state

%start start_state
%%


/*
type program = {
  typedefs : (typedefStruct*ident) list;
  funcdefs : (functionDeclaration*typedefStruct list*stmt list) list
}
*/

start_state:
| type_definition procedure_definition {{typedefs = List.rev $1;funcdefs = List.rev $2}}

/*typdef ? identifier*/
type_definition:
| type_definition TYPEDEF type_spec IDENTIFIER {($3,$4)::$1}
| {[]}

type_spec:
| primitive_type {$1}
| IDENTIFIER {SingleTypeTerm((IdentType $1))}
/*typdef {?} identifier*/
| LEFT_BRACE field_definition RIGHT_BRACE {ListTypeTerm( List.rev $2)}

primitive_type:
| BOOL {SingleTypeTerm(Bool)}
| INT {SingleTypeTerm(Int)}

field_definition:
| rec_field_definition IDENTIFIER COLON type_spec {SingleTypeTermWithIdent($2,$4)::$1}

rec_field_definition:
| field_definition COMMA {$1}
| {[]}








/* At least one procedure required */
/* (functionDeclaration * typedefStruct  * stmt list) list */
procedure_definition:
| rec_procedure_definition PROC procedure_header variable_definition stmt_list END {($3,List.rev $4,$5)::$1}

/* (functionDeclaration * typedefStruct  * stmt list) list */
rec_procedure_definition:
| procedure_definition {$1}
| {[]}

/*type functionDeclaration = (string*funcDecParamList)*/
/*type funcDecParamList = (valRef*typedefStruct*string) list*/
procedure_header:
| IDENTIFIER LEFT_PAREN param RIGHT_PAREN {($1,List.rev $3)}


/*type funcDecParamList = (valRef*typedefStruct*string) list*/
param:
| rec_param param_passing_indicator type_spec IDENTIFIER {($2,$3,$4)::$1}
| {[]}

/*type funcDecParamList = (valRef*typedefStruct*string)*/

rec_param:
| param COMMA {$1}
| {[]}

/* type valRef  */
param_passing_indicator:
| VAL {Val}
| REF {Ref}


/*typedefStruct*/
variable_definition:
| variable_definition type_spec IDENTIFIER SEMICOLON { SingleTypeTermWithIdent($3,$2)::$1 }
| {[]}

/*procedure , stmt list*/

/* stmt list */
stmt_list:
| atomic_stmt SEMICOLON rec_stmt_list {$1::$3}
| compound_stmt rec_stmt_list {$1::$2}

rec_stmt_list:
| stmt_list {$1}
| {[]}

/* stmt */
atomic_stmt:
| lvalue EQ_COL rvalue { Assign($1,$3)}
| READ lvalue { Read($2) }
| WRITE expr { Write($2) }
| IDENTIFIER LEFT_PAREN expr_list RIGHT_PAREN { Method($1,$3) }

/* stmt */
compound_stmt:
| IF expr THEN stmt_list else_block FI {IfDec($2,$4,$5)}
| WHILE expr DO stmt_list OD {WhileDec($2,$4)}


/* object.field*/
lvalue:
| IDENTIFIER { LId($1) }
| lvalue DOT IDENTIFIER { LField($1,$3) }

/* rvalue*/
rvalue:
| expr { Rexpr($1) }
| LEFT_BRACE field_init RIGHT_BRACE { Rstmts(List.rev $2) }
| LEFT_BRACE RIGHT_BRACE { Rempty }

/* rvalue list */
field_init:
| rec_field_init IDENTIFIER EQ rvalue {Rassign($2,$4)::$1}

rec_field_init:
| field_init COMMA {$1}
| {[]}

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
| MINUS expr %prec UMINUS { Eunop(Op_minus,$2) }

/* expr list */
expr_list:
| expr rec_expr_list { $1::$2 }
| {[]}

rec_expr_list:
| COMMA expr_list { $2 }
| {[]}

else_block:
| ELSE stmt_list {$2}
| {[]}

const:
| BOOL_VAL { Ebool($1) }
| INT_VAL { Eint($1) }
| STRING_VAL { Eident($1) }
