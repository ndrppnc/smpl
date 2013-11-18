%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE MODULUS ASSIGN
%token AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token INT FLOAT BOOLEAN CHAR
%token SPAWN PFOR LOCK BARRIER
%token BREAK
%token VOID NULL
%token <int> LITERAL 
%token <float> FLOATING
%token <bool> BOOL
%token <char> CHARACTER
%token <string> STRING
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
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

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
	data_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { dtype = $1
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   INT ID { IntType($2) }
 | FLOAT ID { FloatType($2) }
 | BOOLEAN ID { BoolType($2) }
 | CHAR ID { CharType($2) }
 | STRING ID { StringType($2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }
  
stmt:
    expr SEMI { Expr($1) }
  |	vdecl SEMI { Declare($1) }
  | vdecl ASSIGN expr SEMI { DeclareAssign($1, $3) }
  | RETURN expr SEMI { Return($2) }
  | BREAK expr SEMI { Break() }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | PFOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9, $11) }
  | SPAWN expr SEMI { Spawn($2) }
  | LOCK stmt { Lock($2) }
  | BARRIER expr SEMI { Barrier() }
  
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | FLOATING         { Float($1) }
  | BOOL             { Boolean($1) }
  | CHARACTER        { Char($1) }
  | STRING           { String($1) }
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
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }