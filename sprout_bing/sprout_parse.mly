/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool> BOOL_VAL
%token <int> INT_VAL
%token <string> IDENT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token EQ LT GT
%token PLUS MINUS MUL
%token SEMICOLON
%token EOF

%token COLON
%token <string> IDENTIFIER
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token <string> TYPEDEF
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
| data_structure function_declaration{{typedefs = List.rev $1 ; funcdefs = [1]}}

data_structure:
| data_structure TYPEDEF LEFT_PARENTHESIS typedef_body RIGHT_PARENTHESIS IDENTIFIER {($4,$6)::$1}
| {[]}

typedef_body:
| typedef_body IDENTIFIER COLON type_stmts comma_temp { Printf.printf "in yacc %s " $2;SingleTypeTerm(($2,$4))::$1 }
| typedef_body IDENTIFIER COLON recursive_list_value_init {ListTypeTerm(($2,$4))::$1}
| {[]}



comma_temp:
|COMMA {}
|{}

type_stmts:
|IDENTIFIER { IdentType($1) }
|INT { Int }
|BOOL { Bool }

recursive_list_value_init:
| LEFT_PARENTHESIS typedef_body RIGHT_PARENTHESIS { $2 }



function_declaration:
|function_declaration  function_header function_body END{ ($2,$3)::$1}
| {[]}


function_header:
|PROC IDENTIFIER LEFT_BRACKET param_recursive RIGHT_BRACKET {($2,$4)}

param_recursive:
| param_recursive val_ref type_stmts IDENTIFIER comma_temp {($2,$3,$4)::$1}
| {[]}

/*| argus_recursive argus_type comma_temp {($2,$3,$4)::$1}*/
argus_recursive:
| argus_recursive argus_type comma_temp_2  {}
| {}

comma_temp_2:
|RIGHT_BRACKET {}
|COMMA {}

argus_type:
| expr {}

val_ref:
|VAL {Val}
|REF {Ref}

function_body:
| function_body IDENTIFIER dot_term EQ_DOT assign_term SEMICOLON {}
| function_body type_stmts IDENTIFIER SEMICOLON { } 
| function_body WRITE expr SEMICOLON {  }
| function_body READ IDENTIFIER SEMICOLON{  }
| function_body IDENTIFIER LEFT_BRACKET argus_recursive SEMICOLON {}
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
  | BOOL_VAL {  }
  | INT_VAL {  }
  | IDENTIFIER {  }
  | expr PLUS expr { }
  | expr MINUS expr {  }
  | expr MUL expr  {  }
  | expr EQ expr {  }
  | expr LT expr {  }
  | expr GT expr  {  }
  | MINUS expr %prec UMINUS { }
  | LEFT_BRACKET expr RIGHT_BRACKET {  }

/*
expr:
  | BOOL_VAL { Ebool($1) }
  | INT_VAL { Eint($1) }
  | IDENTIFIER { Eident($1) }
  | expr PLUS expr { Ebinop($1,Op_add,$3) }
  | expr MINUS expr { Ebinop($1,Op_sub,$3) }
  | expr MUL expr  { Ebinop($1,Op_mul,$3) }
  | expr EQ expr { Ebinop($1,Op_eq,$3) }
  | expr LT expr { Ebinop($1,Op_lt,$3) }
  | expr GT expr  { Ebinop($1,Op_gt,$3) }
  | MINUS expr %prec UMINUS { Eunop(Op_minus,$2)}
  | LEFT_BRACKET expr RIGHT_BRACKET { Ebracket(expr) }*/