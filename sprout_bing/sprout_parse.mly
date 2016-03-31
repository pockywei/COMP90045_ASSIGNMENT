/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> IDENT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token EQ LT GT
%token PLUS MINUS MUL
%token SEMICOLON
%token EOF

%token COLON
%token IDENTIFIER
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token TYPEDEF
%token TYPEDEF_VALUE_INIT
%token DOT
%token COMMA
%token END
%token VAL
%token REF
%token LEFT_BRACKET RIGHT_BRACKET
%token WHILE DO OD
%token IF THEN ELSE FI
%token BOOL INT
%token EQ_DOT
%token PROC

%nonassoc EQ LT GT
%left PLUS MINUS
%left MUL DIVIDE
%right ASSIGNMENT
%nonassoc UMINUS

%type <Sprout_ast.program> start_state

%start start_state
%%


start_state:
| data_structure function_declaration{{decls =[("test",Bool)]; stmts = [Test]}}

data_structure:
| data_structure TYPEDEF LEFT_PARENTHESIS typedef_body RIGHT_PARENTHESIS IDENTIFIER {}
| {}

typedef_body:
| typedef_body IDENTIFIER COLON type_stmts comma_temp {}
| typedef_body IDENTIFIER COLON recursive_list_value_init {}
| {}

comma_temp:
|COMMA {}
|{}

type_stmts:
|IDENTIFIER {}
|INT {}
|BOOL {}

recursive_list_value_init:
| LEFT_PARENTHESIS typedef_body RIGHT_PARENTHESIS {}



function_declaration:
|function_declaration  function_header function_body END{}
| {}


function_header:
|PROC IDENTIFIER LEFT_BRACKET param_recursive RIGHT_BRACKET {}

param_recursive:
| param_recursive val_ref type_temp IDENTIFIER comma_temp {}
| {}

val_ref:
|VAL {}
|REF {}

type_temp:
|INT {}
|BOOL {}
|IDENTIFIER {}

function_body:
| function_body IDENTIFIER dot_term EQ_DOT assign_term SEMICOLON{}
| function_body type_temp IDENTIFIER SEMICOLON {} 
| function_body WRITE expr SEMICOLON {}
| function_body READ IDENTIFIER SEMICOLON{}
| function_body IDENTIFIER LEFT_BRACKET RIGHT_BRACKET SEMICOLON {}
| function_body IDENTIFIER LEFT_BRACKET IDENTIFIER RIGHT_BRACKET SEMICOLON {}
| function_body WHILE expr DO function_body OD {}
| function_body IF expr THEN function_body else_stmt FI  {}
| function_body expr SEMICOLON {}
| {}

assign_term:
| expr {}
| LEFT_PARENTHESIS value_assignment_comma RIGHT_PARENTHESIS {}

dot_term:
|{}
|DOT IDENTIFIER {}


value_assignment_comma:
| LEFT_PARENTHESIS value_assignment_comma RIGHT_PARENTHESIS comma_temp{}
| value_assignment_comma IDENTIFIER EQ expr comma_temp {}
| {}


else_stmt:
| ELSE function_body {}
| {}

expr:
  | BOOL { }
  | INT {  }
  | IDENTIFIER {  }
  | expr PLUS expr { }
  | expr MINUS expr {  }
  | expr MUL expr  {  }
  | expr EQ expr {  }
  | expr LT expr {  }
  | expr GT expr  {  }
  | MINUS expr %prec UMINUS { }
  | LEFT_BRACKET expr RIGHT_BRACKET { }










/*
function_declaration:
|function_declaration function_header function_body END {}
| {}



function_header:
| PROC IDENTIFIER LEFT_BRACKET RIGHT_BRACKET {}
| PROC IDENTIFIER LEFT_BRACKET param_recursive RIGHT_BRACKET {}

param_recursive:
| param_recursive val_ref type_temp IDENTIFIER comma_temp {}
| {}

comma_temp:
|COMMA {}
|{}

val_ref:
|VAL {}
|REF {}

type_temp:
|INT {}
|BOOL {}
|IDENTIFIER {}

function_body:
| function_body value_assignment SEMICOLON {}
| function_body type_temp IDENTIFIER SEMICOLON {} 
| function_body WRITE expr SEMICOLON {}
| function_body READ IDENTIFIER SEMICOLON{}
| function_body IDENTIFIER LEFT_BRACKET RIGHT_BRACKET SEMICOLON {}
| function_body IDENTIFIER LEFT_BRACKET IDENTIFIER RIGHT_BRACKET SEMICOLON {}
| function_body WHILE expr DO function_body OD {}
| function_body IF expr THEN function_body else_stmt FI  {}
| function_body expr SEMICOLON {}
| {}

value_assignment:
| IDENTIFIER dot_term EQ_DOT expr {}
| IDENTIFIER dot_term EQ_DOT expr {}
| IDENTIFIER dot_term EQ_DOT LEFT_PARENTHESIS value_assignment_comma RIGHT_PARENTHESIS {}
| {}

value_assignment_eq:
| IDENTIFIER EQ expr {}
| IDENTIFIER EQ LEFT_PARENTHESIS value_assignment_comma RIGHT_PARENTHESIS {}
| {}

dot_term:
|{}
|DOT IDENTIFIER {}

value_assignment_comma:
| value_assignment_comma value_assignment_eq comma_temp {}
| {}

while_normal_stmt:
|function_body {}
|{}

else_stmt:
| ELSE function_body {}
| {}

expr:
  | BOOL { }
  | INT {  }
  | IDENTIFIER {  }
  | expr PLUS expr { }
  | expr MINUS expr {  }
  | expr MUL expr {  }
  | expr EQ expr {  }
  | expr LT expr {  }
  | expr GT expr {  }
  | MINUS expr %prec UMINUS { }
  | LEFT_BRACKET expr RIGHT_BRACKET { }

*/
