open Ast
open String

let includes = 
    "#include <stdio.h>\n" ^
    "#include <stdbool.h>\n" ^
    "#include <string.h>\n\n"

let rec string_of_data_type = function
  IntType(s) -> "int " ^ s
  | FloatType(s) -> "float " ^ s
  | BoolType(s) -> "bool " ^ s
  | CharType(s) -> "char " ^ s
  | StrType(s) -> "char * " ^ s
  | VoidType(s) -> "void " ^ s 

let id_of_data_type = function
  IntType(s) -> s
  | FloatType(s) -> s
  | BoolType(s) -> s
  | CharType(s) -> s
  | StrType(s) -> s
  | VoidType(s) -> s 

let rec string_of_literal = function
  Integer(i) -> string_of_int i
  | Float(f) -> string_of_float f
  | Boolean(b) -> string_of_bool b
  | Char(c) -> Char.escaped c
  | String(s) -> "\"" ^ s ^ "\""


let rec convert_expr = function
  Literal(l) -> string_of_literal l
  | Call(s1, al) -> if (s1 = "print") then  
                ("printf" ^ "(" ^ String.concat "," 
        (List.map (fun e -> convert_expr e ) al) ^ ")")
                    else (s1 ^ "(" ^ String.concat "," 
        (List.map (fun e -> convert_expr e ) al) ^ ")")
 | _ -> ""

let rec convert_stmt = function
    Expr(e) -> convert_expr e ^ ";\n"
 | _ -> ""

let string_of_fdecl (env,fdecl) =
  string_of_data_type fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    ")\n{\n" ^
  String.concat "" (List.map convert_stmt fdecl.body) ^
  "}\n"


let convert_globals (env, globals) = 
    let data_type = fst globals in
    let var_id = id_of_data_type data_type in
    let (global_type, ref_count) = try Semantic_checker.StringMap.find 
    var_id env.Semantic_checker.globals_map with Not_found -> (var_id, 0)  in
    if(ref_count <> 0) then (
    match data_type with
        IntType(s) -> "int " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";\n"
        | FloatType(s) -> "float " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";\n"
        | BoolType(s) -> "bool " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";\n"
        | CharType(s) -> "char " ^ s ^ " = " ^ "'" ^ string_of_literal (snd globals) ^ "'"  ^ ";\n"
        | StrType(s) -> "char " ^ s ^ "[" ^ 
                        string_of_int (String.length (string_of_literal(snd globals)) - 1) 
                        ^ "]" ^ " = " ^ string_of_literal (snd globals) ^ ";\n"
        | VoidType(s) -> ""
    ) else ("")

let rec append_env env = function
    [] -> []
    | hd::tl -> (env, hd)::append_env env tl

let generate_code (vars, funcs) env =
    let env_vars = append_env env vars in
    let env_funcs = append_env env funcs in
    "/* Code Generated from SMPL */\n\n" ^ includes ^ 
    String.concat "" (List.map convert_globals env_vars) ^ ";\n\n" 
  ^ String.concat "\n\n" (List.map string_of_fdecl env_funcs)
