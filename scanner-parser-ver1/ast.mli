type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type data_type =
	IntType of string
  | FloatType of string
  | BoolType of string
  | CharType of string
  | VoidType of string

type literal = 
    Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  
type expr =
    Literal of literal
  | Id of string
  | Declare of data_type
  | DeclareAssign of data_type * expr
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
  | If of expr * stmt * stmt
  | Else of stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Pfor of expr * expr * expr * expr * stmt
  | Spawn of expr
  | Lock of stmt
  | Barrier of expr	
  
type func_decl = {
	dtype : data_type;
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }
  
type program = string list * func_decl list