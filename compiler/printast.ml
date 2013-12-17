open Ast

let string_of_data_type = function
  IntType(s) -> "Integer-" ^ s
  | FloatType(s) -> "Float-" ^ s
  | BoolType(s) -> "Boolean-" ^ s
  | CharType(s) -> "Char-" ^ s
  | StrType(s) -> "Str-" ^ s
  | VoidType(s) -> "Void-" ^ s 

let string_of_literal = function
  Integer(i) -> "Integer Lit: " ^ string_of_int i
  | Float(f) -> "Float Lit: " ^ string_of_float f
  | Boolean(b) -> "Boolean Lit: " ^ string_of_bool b
  | Char(c) -> "Char Lit: " ^ Char.escaped c
  | String(s) -> "String Lit: \"" ^ s ^ "\""

let rec string_of_expr = function
  Literal(l) -> string_of_literal l
  | Id(s) -> "ID:" ^ s
  | Paren(s) -> "(" ^ string_of_expr s ^ ")"
  | Binop(e1, o, e2) -> "BINOP:" ^ string_of_expr e1 ^ " " ^
      (match o with
	Add -> "PLUS"
      | Sub -> "MINUS"
      | Mult -> "TIMES"
      | Div -> "DIV"
      | Mod -> "MOD"
      | Equal -> "EQUAL"
      | Neq -> "NOTEQUAL"
      | Greater -> "GT"
      | Geq -> "GTE"
      | Less -> "LT"
      | Leq -> "LTE"
      | And -> "AND"
      | Or -> "OR") ^ " " ^ string_of_expr e2
  | Not(e1) -> "BINOP:" ^ "NOT" ^ string_of_expr e1
  | Pp(id) -> "INCREMENT " ^ id
  | Mm(id) -> "DECREMENT " ^ id
  | Assign(id, idx) -> "ASSIGN- " ^ 
      (match id with
        Id(id) -> id
        | _ -> "ERROR") ^ "to: [" ^ string_of_expr idx ^ "]"
  | Call(s1, al) -> "Call: " ^ s1 ^ ": (" ^ String.concat "," 
        (List.map (fun e -> string_of_expr e ) al) ^ ")"
  | Noexpr -> "NoExpr\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(e) -> "EXPR: [" ^ string_of_expr e ^ "]\n"
  | Return(e) -> "RETURN " ^ string_of_expr e ^ ";\n"
  | Break(e) -> "BREAK " ^ string_of_expr e ^ ";\n"
  | Continue(e) -> "CONTINUE " ^ string_of_expr e ^ ";\n"
  | Declare(d) -> "Declare " ^ string_of_data_type d ^ ";\n"
  | DeclareAssign(d, e) -> "DeclareAssign " ^ string_of_data_type d ^ 
                            ": AND Assign: " ^ string_of_expr e ^ ";\n"
  | If (p, t, f) -> 
        "IF " ^ string_of_expr p 
				    ^ " THEN DO "
            ^ string_of_stmt t
            ^ " ELSE DO " ^ string_of_stmt f ^ ";\n"
  | For (s, e, se, b) -> 
		   "DECLARE: " ^ string_of_expr s
	          ^ " AND DO " ^ string_of_stmt b
						^ " WHILE " ^ string_of_expr e 
						^ " PERFORMING " ^ string_of_expr se ^ ";\n"
  | While (e, b) -> " WHILE " ^ string_of_expr e ^ " DO " ^ string_of_stmt b ^ ";\n"
  | Pfor(t, c, s, e, b) -> "PFOR: Threads: " ^ string_of_expr t ^ "; Variable: "
  ^ string_of_expr c ^ " " ^ string_of_expr s   
	          ^ " AND DO " ^ string_of_stmt b
						^ " WHILE " ^ string_of_expr e ^ ";\n"
  | Spawn(e) -> "Spawn: " ^ string_of_expr e ^ "\n"
  | Lock(s) -> "Lock: " ^ string_of_stmt s ^ "\n"
  | Barrier(e) -> "Barrier: " ^ string_of_expr e ^ "\n"


  let string_of_vdecl id = "long " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "FUNCTION " ^ string_of_data_type fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


let string_of_globals globals = 
  "GLOBALS " ^ string_of_data_type (fst globals) ^ " = " ^ string_of_literal (snd globals)

let parse_program (vars, funcs) =
  "Vars: \n" ^ String.concat ";\n" (List.map string_of_globals vars) ^ "\n" 
  ^ "Funcs: \n" ^ String.concat "\n" (List.map string_of_fdecl funcs)
