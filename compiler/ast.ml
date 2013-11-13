type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type data_type =
    IntType of string
  | FloatType of string
  | BoolType of string
  | CharType of string
  | StrType of string
  | VoidType of string


type literal =
    Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string

type expr =
  Id of string
  | Literal of literal
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr
(* Add Array *)

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Break of expr
  | Declare of data_type
  | DeclareAssign of data_type * expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Pfor of expr * expr * expr * expr * stmt
  | Spawn of stmt (* Not Sure whether stmt or expr *)
  | Lock of stmt (* Not sure whether stmt or expr *)
  | Barrier of expr	

type func_decl = {
    fname : data_type;
    formals : data_type list;
    locals : data_type list;
    body : stmt list;
  }

type program = (data_type * literal) list * func_decl list

let rec string_of_data_type = function
  IntType(s) -> "Integer-" ^ s
  | FloatType(s) -> "Float-" ^ s
  | BoolType(s) -> "Boolean-" ^ s
  | CharType(s) -> "Char-" ^ s
  | StrType(s) -> "Str-" ^ s
  | VoidType(s) -> "Void-" ^ s 

let rec string_of_literal = function
  Integer(i) -> "Integer Lit: " ^ string_of_int i
  | Float(f) -> "Float Lit: " ^ string_of_float f
  | Boolean(b) -> "Boolean Lit: " ^ string_of_bool b
  | Char(c) -> "Char Lit: " ^ Char.escaped c
  | String(s) -> "String Lit: \"" ^ s ^ "\""

let rec string_of_expr = function
  Literal(l) -> string_of_literal l
  | Id(s) -> "ID:" ^ s
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
  | Assign(id,idx) -> "ASSIGN- " ^ id ^ "to: [" ^ string_of_expr idx ^ "]"
  | Call(s1, al) -> "Call: " ^ s1 ^ ": (" ^ String.concat "," 
        (List.map (fun e -> string_of_expr e ) al) ^ ")"
  | Noexpr -> "NoExpr\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(e) -> "EXPR: [" ^ string_of_expr e ^ "]\n"
  | Return(e) -> "RETURN " ^ string_of_expr e ^ ";\n"
  | Break(e) -> "BREAK " ^ string_of_expr e ^ ";\n"
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
  | Pfor(t, s, e, se, b) -> "PFOR: Threads: " ^ string_of_expr t ^ "; " ^ string_of_expr s   
	          ^ " AND DO " ^ string_of_stmt b
						^ " WHILE " ^ string_of_expr e 
						^ " PERFORMING " ^ string_of_expr se ^ ";\n"
  | Spawn(s) -> "Spawn: " ^ string_of_stmt s ^ "\n"
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

let string_of_program (vars, funcs) =
  "Vars: \n" ^ String.concat ";\n" (List.map string_of_globals vars) ^ "\n" 
  ^ "Funcs: \n" ^ String.concat "\n" (List.map string_of_fdecl funcs)
