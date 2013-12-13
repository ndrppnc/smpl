{ open Parser 
  open Str
}

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
| "++"	    { PP }
| "--"	    { MM }
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }
| "break"   { BREAK }
| "continue"{ CONTINUE }
| "spawn"   { SPAWN }
| "lock"    { LOCK }
| "barrier" { BARRIER }
| "pfor"    { PFOR }
| "int"     { INT }
| "float"   { FLOAT }
| "boolean" { BOOLEAN }
| "char"    { CHAR }
| "string"  { STRING }
| "void"    { VOID }
| "null"    { NULL }
| "true"  as lxm { BOOL_LIT(bool_of_string lxm) }
| "false" as lxm { BOOL_LIT(bool_of_string lxm) }
| ['0'-'9']+ as lit { INTEGER_LIT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']*('e'['-''+']?['0'-'9']+)? as lit { FLOAT_LIT(float_of_string lit) }
| '.'['0'-'9']+('e'['-''+']?['0'-'9']+)? as lit { FLOAT_LIT(float_of_string lit) }
| ['0'-'9']+'e'['-''+']?['0'-'9']+ as lit { FLOAT_LIT(float_of_string lit) }
| '''(_)''' as lxm { CHAR_LIT(lxm.[1]) }
| '"'([^'"''\\']*('\\'_[^'"''\\']*)* as lxm)'"' { STRING_LIT(lxm) } 
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
