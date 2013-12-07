open Ast

module VarMap = Map.Make(struct
                type t = string * string
                let compare x y = Pervasives.compare x y
        end)


module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    globals_map     : (string * int) StringMap.t; (* global variable name-> type, ref_count *)
    functions_map   : (string * int) StringMap.t; (* function name -> return type, ref_count *)
    formals_map     : (string) VarMap.t; (* function name:argument name -> types *)
    locals_map      : (string * int) VarMap.t; (*function name:local variable name -> type, ref_count *)
    valid_syntax    : bool

  }


let print_debug = false

let validate_program (globalvars, funcs) =
    let id_of_data_type = function
        IntType(s) -> s
        | FloatType(s) -> s
        | BoolType(s) -> s
        | CharType(s) -> s
        | StrType(s) -> s
        | VoidType(s) -> s 

    in let get_data_type = function
        IntType(s) -> "integer"
        | FloatType(s) -> "float"
        | BoolType(s) -> "boolean"
        | CharType(s) -> "char"
        | StrType(s) -> "string"
        | VoidType(s) -> "void"

    in let global_map =
        List.fold_left (fun m global -> StringMap.add 
        (id_of_data_type(fst global)) (get_data_type(fst global), 1) m)
        StringMap.empty globalvars
    in let function_map =
        List.fold_left (fun m func -> StringMap.add
        (id_of_data_type(func.fname)) (get_data_type(func.fname), 1) m) 
        StringMap.empty funcs
    in let global_env = {
        globals_map = global_map;
        functions_map = function_map;
        formals_map = VarMap.empty;
        locals_map = VarMap.empty;
        valid_syntax = false;
    }
    in let string_of_data_type = function
        IntType(s) -> "integer variable " ^ s
        | FloatType(s) -> "float variable " ^ s
        | BoolType(s) -> "boolean variable " ^ s
        | CharType(s) -> "char variable " ^ s
        | StrType(s) -> "string variable " ^ s
        | VoidType(s) -> "void variable " ^ s 
    
    in let string_of_literal = function
        Integer(i) -> "integer value: " ^ string_of_int i
        | Float(f) -> "float value: " ^ string_of_float f
        | Boolean(b) -> "boolean value: " ^ string_of_bool b
        | Char(c) -> "char value: " ^ Char.escaped c
        | String(s) -> "string value: \"" ^ s ^ "\""
        
    in let get_literal_type = function
        Integer(s) -> "integer"
        | Float(s) -> "float"
        | Boolean(s) -> "boolean"
        | Char(s) -> "char"
        | String(s) -> "string"

        
    in let rec valid_syntax = function
        [] -> true
        | hd :: tl ->
            if hd then
                valid_syntax tl
            else
                false
        
    in let validate_initialized variable =
        if (get_data_type (fst variable) = get_literal_type (snd variable)) then
            if (print_debug) then (
                let _ = print_string ("Global " ^ string_of_data_type (fst variable) ^ " is valid\n")
                in true)
            else (
                true)
        else (
            raise (Failure ("Global " ^ string_of_data_type (fst variable)
                ^ " is assigned a " ^ string_of_literal (snd variable))))
    in let process_local_var fname_id var env = 
        { env with locals_map = 
            VarMap.add (fname_id, id_of_data_type var) ((get_data_type var), 1)
            env.locals_map } 
    
    in let process_formals fname_id env arg = 
        { env with formals_map =
            VarMap.add (fname_id, id_of_data_type arg) (get_data_type arg)
            env.formals_map }

    in let validate_statement fname env = function
        Declare(d) -> 
            let _ = if (print_debug) then (
                print_string (id_of_data_type fname ^ " Local Variable " ^
                string_of_data_type d ^ "\n"))
            in process_local_var (id_of_data_type fname) d env
        |_ -> env 
    in let validate_funcs curr_env functions =
        let _ = if (print_debug) then (
            print_string ("Validate function: " ^ 
            string_of_data_type functions.fname ^ "\n"))
        in let new_env = 
            List.fold_left (fun env argument -> 
                process_formals (id_of_data_type functions.fname) env argument)
            curr_env functions.formals in
        List.fold_left (fun env line -> 
            validate_statement functions.fname env line) new_env functions.body
    
    (* For debugging *)
    in let print_global_map env =
        StringMap.iter (fun dt_key (vartype, ref) -> print_string ("Map var: id: " ^ 
        dt_key ^ " type=" ^ vartype ^ " ref=" ^ (string_of_int ref) ^ "\n"))
        env.globals_map
    in let print_function_map env = 
        StringMap.iter (fun dt_key (vartype, ref) -> print_string ("Map function: id: " ^ 
        dt_key ^ " type=" ^ vartype ^ " ref=" ^ (string_of_int ref) ^ "\n"))
        env.functions_map
    in let print_locals_map env = 
        VarMap.iter (fun (dt_func, dt_var) (var_type, var_ref) ->
            print_string ("1. Func: " ^ dt_func ^ " var: " ^ dt_var ^ " type: " ^
            var_type ^ " ref= " ^ string_of_int var_ref ^ "\n")
        ) env.locals_map
    in let print_formals_map env =
        VarMap.iter (fun (dt_func, dt_arg) (arg_type) ->
            print_string ("2. Func: " ^ dt_func ^ " arg: " ^ dt_arg ^ " type: " ^
            arg_type ^ "\n")
        ) env.formals_map
    
    in let final_env curr_env valid =
        { curr_env with valid_syntax = valid }

    in if valid_syntax (List.map validate_initialized globalvars) then
        let curr_env = List.fold_left (fun env func_list -> 
            validate_funcs env func_list) global_env funcs in
        if (print_debug) then (
            let _ = print_global_map curr_env in
            let _ = print_function_map curr_env in
            let _ = print_locals_map curr_env in
            let _ = print_formals_map curr_env
        in final_env curr_env true)
        else (
            final_env curr_env true)
    else
        final_env global_env false
