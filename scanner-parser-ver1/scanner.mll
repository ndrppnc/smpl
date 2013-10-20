{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
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
| "!"	    { NOT }
| "&&"	    { AND }
| "||"	    { OR }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| ">"       { GT }
| ">="      { GEQ }
| "while"   { WHILE }
| "return"  { RETURN }
| "break"   { BREAK }
| "spawn"   { SPAWN }
| "lock"    { LOCK }
| "barrier" { BARRIER }
| "int"     { INT }
| "void"    { VOID }
| "null"    { NULL }
| "true"  as lxm { BOOL(bool_of_string lxm) }
| "false" as lxm { BOOL(bool_of_string lxm) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']*('e'['-''+']?['0'-'9']+)? as lit { FLOAT(float_of_string lit) }
| '.'['0'-'9']+('e'['-''+']?['0'-'9']+)? as lit { FLOAT(float_of_string lit) }
| ['0'-'9']+'e'['-''+']?['0'-'9']+ as lit { FLOAT(float_of_string lit) }
| '''['a'-'z']''' as lxm { CHAR(lxm.[1]) }
| '"'([^'"''\\']*('\\'.[^'"''\\']*)*)'"' as lxm { STRING(lxm) } 
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
