open Ast

let rec string_of_data_type = function
  IntType(s) -> "integer variable " ^ s
  | FloatType(s) -> "float variable " ^ s
  | BoolType(s) -> "boolean variable " ^ s
  | CharType(s) -> "char variable " ^ s
  | StrType(s) -> "string variable " ^ s
  | VoidType(s) -> "void variable " ^ s 

let get_data_type = function
  IntType(s) -> "integer"
  | FloatType(s) -> "float"
  | BoolType(s) -> "boolean"
  | CharType(s) -> "char"
  | StrType(s) -> "string"
  | VoidType(s) -> "void"


let rec string_of_literal = function
  Integer(i) -> "integer value: " ^ string_of_int i
  | Float(f) -> "float value: " ^ string_of_float f
  | Boolean(b) -> "boolean value: " ^ string_of_bool b
  | Char(c) -> "char value: " ^ Char.escaped c
  | String(s) -> "string value: \"" ^ s ^ "\""

let get_literal_type = function
  Integer(s) -> "integer"
  | Float(s) -> "float"
  | Boolean(s) -> "boolean"
  | Char(s) -> "char"
  | String(s) -> "string"

let valid_syntax = ref true

let validate_globals globals =
    let var_type = get_data_type (fst globals) in 
    let lit_type = get_literal_type (snd globals) in
        if (var_type = lit_type) then
            print_string ""(*print_string ("Global " ^ string_of_data_type (fst globals) ^ "
            is valid\n")*)
        else (
            valid_syntax := false;
            raise (Failure ("Global " ^ string_of_data_type (fst globals)
                ^ " is assigned a " ^ string_of_literal (snd globals))))

let validate_program (vars, funcs) =
    List.iter validate_globals vars;
    (*let valid_funcs = (List.map validate_fdecl funcs) in*)
    (*print_string "Validation is complete\n";*)
    valid_syntax
