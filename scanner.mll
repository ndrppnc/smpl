{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"      { comment lexbuf }           (* Comments *)
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| ';'       { SEMI }
| ','       { COMMA }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '%'       { MODULUS }
| '='       { ASSIGN }
| "!"		{ NOT }
| "&&"		{ AND }
| "||"		{ OR }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| ">"       { GT }
| ">="      { GEQ }
| "while"   { WHILE }
| "return"  { RETURN }
| "break"	{ BREAK }
| "spawn"   { SPAWN }
| "lock"    { LOCK }
| "barrier" { BARRIER }
| "int"     { INT }
| "float"   { FLOAT }
| "boolean" { BOOLEAN }
| "char"    { CHAR }
| "string"  { STRING }
| "void"    { VOID }
| "null"	{ NULL }
| "true"	{ bool $1 }
| "false"	{ bool $1 }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| [float regex] as lxm { FLOATING(float_of_string lxm) }
| '''['a'-'z']''' as lxm { CHAR(lxm) }
| '"'([^'"''\']*(?:'\'.[^'"''\']*)*)'"' as lxm { STRING(lxm) } (* "([^"\\]*(?:\\.[^"\\]*)*)" *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*('['['0'-'9']+']')+ as lxm { ARRAY(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
