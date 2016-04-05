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
%left NOT
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left MUL DIV
%right EQ_COL
%nonassoc UMINUS

%type <Sprout_ast.program> start_state

%start start_state
%%


start_state:
| type_definition procedure_definition {{typedefs=[1];funcdefs=[2]}}

type_definition:
| type_definition TYPEDEF type_spec IDENTIFIER {}
| {}

type_spec:
| primitive_type {}
| IDENTIFIER {}
| LEFT_BRACE field_definition RIGHT_BRACE {}

primitive_type:
| BOOL {}
| INT {}

field_definition:
| rec_field_definition IDENTIFIER COLON type_spec {}

rec_field_definition:
| field_definition COMMA {}
| {}

/* At least one procedure required */
procedure_definition:
| rec_procedure_definition PROC procedure_header variable_definition procedure_body END {}

rec_procedure_definition:
| procedure_definition {}
| {}

procedure_header:
| IDENTIFIER LEFT_PAREN param RIGHT_PAREN {}

param:
| rec_param param_passing_indicator type_spec IDENTIFIER {}
| {}

rec_param:
| param COMMA {}
| {}

param_passing_indicator:
| VAL {}
| REF {}

variable_definition:
| variable_definition type_spec IDENTIFIER SEMICOLON{}
| {}

procedure_body:
| rec_procedure_body atomic_stmt SEMICOLON {}
| rec_procedure_body compound_stmt {}

rec_procedure_body:
| procedure_body {}
| {}

atomic_stmt:
| lvalue EQ_COL rvalue {}
| READ lvalue {}
| WRITE expr {}
| IDENTIFIER LEFT_PAREN expr_list RIGHT_PAREN {}

compound_stmt:
| IF expr THEN stmt_list else_block FI {}
| WHILE expr DO stmt_list OD {}

lvalue:
| IDENTIFIER {}
| lvalue DOT IDENTIFIER {}

rvalue:
| expr {}
| LEFT_BRACE field_init RIGHT_BRACE {}

field_init:
| rec_field_init IDENTIFIER EQ rvalue {}

rec_field_init:
| field_init COMMA {}
| {}

expr:
| lvalue {}
| const {}
| LEFT_PAREN expr RIGHT_PAREN {}
| expr binop expr {}
| unop expr {}

expr_list:
| rec_expr_list expr {}
| {}

rec_expr_list:
| expr COMMA {}
| {}

stmt_list:
| procedure_body {}

else_block:
| ELSE stmt_list {}
| {}

binop:
| PLUS {}
| MINUS {}
| MUL {}
| DIV {}
| EQ {}
| NEQ {}
| LT {}
| GT {}
| LTE {}
| GTE {}
| AND {}
| OR {}

unop:
| NOT {}
| UMINUS {}

const:
| BOOL_VAL {}
| INT_VAL {}
| STRING_VAL {}
