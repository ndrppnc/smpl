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
  | Not of expr
  | Pp of string
  | Mm of string
  | Assign of expr * expr
  | Call of string * expr list
  | Paren of expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Break of expr
  | Continue of expr
  | Declare of data_type
  | DeclareAssign of data_type * expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Pfor of expr * expr * expr * expr * stmt
  | Spawn of expr
  | Lock of stmt
  | Barrier of expr	

type func_decl = {
    fname : data_type;
    formals : data_type list;
    body : stmt list;
  }

type program = (data_type * literal) list * func_decl list
