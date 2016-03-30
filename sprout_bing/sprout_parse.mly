/* ocamlyacc parser for bean */


/*

function_declaration:
|function_declaration function_header function_body END {}
| {}


function_header:
| IDENTIFIER LEFT_PARENTHESIS RIGHT_PARENTHESIS {}
| IDENTIFIER LEFT_PARENTHESIS param_recursive param RIGHT_PARENTHESIS {}

param:
| VAL IDENTIFIER {}
| REF IDENTIFIER {}

param_recursive:
| param_recursive VAL IDENTIFIER COMMA {}
| param_recursive REF IDENTIFIER COMMA {}
| {}

function_body:
| function_body normal_stmt {}
| {}

normal_stmt:
| value_assignment SEMICOLON {}
| IDENTIFIER IDENTIFIER SEMICOLON {} 
| WRITE expr SEMICOLON {}
| READ IDENTIFIER {}
| IDENTIFIER LEFT_BRACKET RIGHT_BRACKET SEMICOLON {}
| while_stmt {}
| if_stmt {}
| expr SEMICOLON {}
| {}

value_assignment:
| IDENTIFIER dot_optional EQ_DOT expr {}
| IDENTIFIER dot_optional EQ_DOT LEFT_PARENTHESIS value_assignment_dot value_assignment RIGHT_PARENTHESIS {}
| {}

dot_optional:
| DOT {}
| {}

value_assignment_dot:
| value_assignment_dot value_assignment COMMA {}
| {}

while_stmt:
|WHILE expr DO normal_stmt OD {}

if_stmt:
|IF expr THEN normal_stmt else_stmt FI  {}

else_stmt:
| ELSE normal_stmt {}
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
  | MINUS expr %prec UMINUS { }
  | LEFT_BRACKET expr RIGHT_BRACKET { }
*/
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

%nonassoc EQ LT
%left PLUS MINUS
%left MUL
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
|function_declaration function_header function_body END {}
| {}


/*abc() or abc(, , ,)*/
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
  /* Binary operators */
  | expr PLUS expr { }
  | expr MINUS expr {  }
  | expr MUL expr {  }
  | expr EQ expr {  }
  | expr LT expr {  }
  | expr GT expr {  }
  /*MINUS expr has same presidence as UMINUS*/
  | MINUS expr %prec UMINUS { }
  | LEFT_BRACKET expr RIGHT_BRACKET { }




