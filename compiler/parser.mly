%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS PP MM TIMES DIVIDE MODULUS ASSIGN
%token AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token INT FLOAT BOOLEAN CHAR STRING
%token SPAWN PFOR LOCK BARRIER
%token BREAK CONTINUE
%token VOID NULL
%token <int> INTEGER_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> ID
%token GLOBAL
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

%start program
%type < Ast.program> program

%%

program:
   /* nothing */ { [], [] }
  | program fdecl { fst $1, ($2 :: snd $1)} /* devashi: What the heck is this? */
  | program vdecl ASSIGN type_literal SEMI { ( ($2,$4) :: fst $1), snd $1 }  /* devashi: What the heck is this? */

fdecl:
	ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
        {{ fname = VoidType($1); formals = $3; body = List.rev $6 }}
  | VOID ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    {{ fname = VoidType($2); formals = $4; body = List.rev $7 }}
  | vdecl LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    {{ fname = $1; formals = $3; body = List.rev $6 }}

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
  vdecl { [$1] }
  | formal_list COMMA vdecl { $3 :: $1 }

vdecl:
   INT ID { IntType($2) }
 | FLOAT ID { FloatType($2) }
 | BOOLEAN ID { BoolType($2) }
 | CHAR ID { CharType($2) }
 | STRING ID { StrType($2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | vdecl SEMI { Declare($1) }
  | vdecl ASSIGN expr SEMI { DeclareAssign($1, $3) }
  | RETURN expr_opt SEMI { Return($2) }
  | BREAK empty_opt SEMI { Break($2) }
  | CONTINUE empty_opt SEMI { Continue($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | PFOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { Pfor($3, $5, $7, $9, $11) }
  | SPAWN call_opt SEMI { Spawn($2) }
  | LOCK stmt { Lock($2) }
  | BARRIER empty_opt SEMI { Barrier($2) }

call_opt:
  ID LPAREN actuals_opt RPAREN { Call($1, $3) }

empty_opt:
    /* nothing */ { Noexpr }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

type_literal:
  INTEGER_LIT            { Integer($1) }
  | FLOAT_LIT            { Float($1) }
  | BOOL_LIT             { Boolean($1) }
  | CHAR_LIT             { Char($1) }
  | STRING_LIT           { String($1) }

expr:
    ID             { Id($1) }
  | ID PP	   { Pp($1) }
  | ID MM	   { Mm($1) }
  | type_literal   { Literal($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MODULUS expr { Binop($1, Mod,   $3) }
  | expr AND     expr { Binop($1, And, $3) }
  | expr OR     expr { Binop($1, Or, $3) }
  | NOT expr         { Not($2) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN   expr { Assign(Id($1), $3) }
  | LPAREN expr RPAREN { Paren($2) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
