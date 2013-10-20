%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE MODULUS ASSIGN
%token AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token SPAWN PFOR LOCK BARRIER
%token BREAK
%token VOID NULL
%token <int> LITERAL 
%token <float> FLOAT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <string> ID
%token EOF

%left COMMA


%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS 
%left NOT

%start expr
%type < Ast.expr> expr

%%

expr:
  LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MODULUS expr { Binop($1, Mod,   $3) }
  | expr AND     expr { Binop($1, And, $3) }
  | expr OR     expr { Binop($1, Or, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | FLOAT            { Float($1) }
  | BOOL             { Boolean($1) }
  | CHAR             { Char($1) }
  | STRING           { String($1) }
