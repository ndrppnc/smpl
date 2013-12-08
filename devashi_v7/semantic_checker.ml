open Ast

module VarMap = Map.Make(struct
                type t = string * string
                let compare x y = Pervasives.compare x y
        end)


module StringMap = Map.Make(String)

(* Symbol table: Information about all the symbols *)
type env = {
    globals_map     : (string * int) StringMap.t; (* global variable name-> type, ref_count *)
    functions_map   : (string * int) StringMap.t; (* function name -> return type, ref_count *)
    formals_map     : (string) VarMap.t; (* function name:argument name -> types *)
    locals_map      : (string * int) VarMap.t; (*function name:local variable name -> type, ref_count *)
    unvisited_funcs : string list; (* maintains function call stack for getting ref_count *)
    visited_funcs   : string list; (* maintains list of visited functions *)
    visited_all     : bool;
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
        List.fold_left (fun m global -> 
            let dt_global = id_of_data_type(fst global) in
            let found = StringMap.mem dt_global m in
            if(found = false) then
                (StringMap.add dt_global (get_data_type(fst global), 0) m)
            else
                raise (Failure ("global variable " ^ dt_global ^ 
                " is redefined"))
        ) StringMap.empty globalvars
    in let function_map =
        List.fold_left (fun m func -> 
            let id_func = id_of_data_type(func.fname) in
            let found = StringMap.mem id_func m in
            if(found = true) then
                raise (Failure ("Function " ^ id_func ^ " is redefined"))
            else (
                let found = StringMap.mem id_func global_map in
                if(found = false) then
                    (StringMap.add id_func (get_data_type(func.fname), 0) m)
                else
                    raise (Failure ("Function " ^ id_func ^ 
                    " conflicts with a global variable"))
            )
        ) StringMap.empty funcs
    in let func_decl_map = 
        List.fold_left (fun m func -> StringMap.add
        (id_of_data_type(func.fname)) func m)
        StringMap.empty funcs
    in let global_env = {
        globals_map = global_map;
        functions_map = function_map;
        formals_map = VarMap.empty;
        locals_map = VarMap.empty;
        unvisited_funcs = ["main"];
        visited_funcs = [];
        visited_all = false;
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
    in let print_unvisited_funcs env =
        List.iter (fun f -> print_string ("unvisited: "^f);print_newline()) env.unvisited_funcs
    in let print_visited_funcs env = 
        List.iter (fun f -> print_string ("visited: "^f);print_newline()) env.visited_funcs
    

    in let validate_initialized variable =
        if (get_data_type (fst variable) = get_literal_type (snd variable)) then
            if (print_debug) then (
                print_string ("Global " ^ string_of_data_type (fst variable) ^ " is valid\n"))
            else (
                print_string (""))
        else (
            raise (Failure ("Global " ^ string_of_data_type (fst variable)
                ^ " is assigned a " ^ string_of_literal (snd variable))))
    in let process_local_var fname_id var env = 
        let id_var = id_of_data_type var in
        let found = VarMap.mem (fname_id, id_var) env.locals_map in
        if(found = false) then
            { env with locals_map = 
                VarMap.add (fname_id, id_var) ((get_data_type var), 0)
                env.locals_map } 
        else
            raise (Failure ("Function " ^ fname_id ^ 
            " contains more than one local variable with the name " ^ id_var))
    
    in let process_formals fname_id arg env = 
        let id_arg = id_of_data_type arg in
        let found = VarMap.mem (fname_id, id_arg) env.formals_map in
        if(found = false) then
            { env with formals_map =
                VarMap.add (fname_id, id_arg) (get_data_type arg)
                env.formals_map }
        else
            raise (Failure ("Function " ^ fname_id ^
            " contains more than one argument with the name " ^ id_arg))
    
    in let rec process_expr fname_id env = function
        Id(s) -> 
            let (var_type, ref_count) = try VarMap.find (fname_id, s) env.locals_map
                           with Not_found -> ("", 0) in
            if(var_type = "") then (
                let var_type  = try VarMap.find (fname_id, s) env.formals_map 
                                with Not_found -> "" in
                if(var_type = "") then (
                    let (var_type, ref_count) = try StringMap.find s env.globals_map
                                with Not_found -> ("", 0) in
                    if(var_type = "") then (
                        raise (Failure ("Variable '" ^ s ^ "' " ^ "in function
                            '" ^ fname_id ^ "' is not defined"))
                    ) else (
                        { env with globals_map = 
                            StringMap.add s (var_type, ref_count+1) env.globals_map
                        }
                    ))
                else (
                    env
                )
            ) else (
            { env with locals_map = 
                VarMap.add (fname_id, s) (var_type, ref_count+1)
                env.locals_map } 
            )
        | Call(s1, al) ->
                let (var_type, ref_count) = try StringMap.find s1 env.functions_map
                                            with Not_found -> ("", 0) in
                if(var_type = "" && (s1 <> "printf")) then
                    raise (Failure ("Function '" ^ s1 ^ "' is not defined"))
                else (
                    if(List.mem s1 env.visited_funcs = false) then (
                        let env = { env with  unvisited_funcs =
                            if((List.mem s1 env.unvisited_funcs = false) 
                            && s1 <> "printf") then 
                                s1::env.unvisited_funcs
                            else
                                env.unvisited_funcs 
                            } in
                        let mod_env = { env with functions_map = 
                            StringMap.add s1 (var_type, ref_count+1) env.functions_map
                        } 
                        in List.fold_left (fun old_env arg -> 
                        process_expr fname_id old_env arg) mod_env al
                    ) else (
                        List.fold_left (fun old_env arg -> 
                            process_expr fname_id old_env arg) env al
                    )
                )
        |_ -> env 


    in let process_statement fname env = function
        Declare(d) -> 
            let _ = if (print_debug) then (
                print_string (id_of_data_type fname ^ " Local Variable " ^
                string_of_data_type d ^ "\n"))
            in process_local_var (id_of_data_type fname) d env
        |_ -> env 
    
    in let update_env_map curr_env functions =
        let _ = if (print_debug) then (
            print_string ("Validate function: " ^ 
            id_of_data_type functions.fname ^ "\n"))
        in let new_env = 
            List.fold_left (fun env argument -> 
                process_formals (id_of_data_type functions.fname) argument env)
            curr_env functions.formals in
        List.fold_left (fun env line -> 
            process_statement functions.fname env line) new_env functions.body
    
    in let process_func_body fname env = function
        Expr(e) -> process_expr (id_of_data_type fname) env e
        |_ -> env 
    
    in let process_func env =
        let fname_id = 
            match env.unvisited_funcs with 
            [] -> ""
            | hd::tl -> hd
        in if(fname_id = "") then
            { env with visited_all = true }
        else if(fname_id <> "printf") then (
            let env = if(fname_id = "main") then
                let (var_type, ref_count) = try StringMap.find fname_id
                env.functions_map with Not_found -> raise (Failure ("function '" ^
                fname_id ^ "' not defined")) in
                { env with functions_map = 
                    StringMap.add fname_id (var_type, ref_count+1) env.functions_map}
            else
                env
            in let fdecl = try StringMap.find fname_id func_decl_map
                        with Not_found -> 
                            raise (Failure ("function '" ^ fname_id ^ "' not defined"))
            in let p_env = { env with  unvisited_funcs = List.tl env.unvisited_funcs }
            in let p_env = { p_env with visited_funcs = fname_id :: p_env.visited_funcs } in
            List.fold_left (fun env_temp line ->
                process_func_body fdecl.fname env_temp line) p_env fdecl.body
        ) else (
            env
        )
    
    in let rec process_code env =
        let new_env = process_func env in
        if(new_env.visited_all = false) then
            let _ = if(print_debug) then (
                print_string ("process_code\n");
                print_unvisited_funcs new_env;
                print_visited_funcs new_env
            )
            in process_code new_env
        else
            new_env

    in let final_env curr_env valid =
        { curr_env with valid_syntax = valid }

    in let _ = List.iter validate_initialized globalvars
    in let first_env = List.fold_left (fun env func_list -> 
            update_env_map env func_list) global_env funcs 
    in let second_env = process_code first_env in 
        if (print_debug) then (
            let _ = print_global_map second_env in
            let _ = print_function_map second_env in
            let _ = print_locals_map second_env in
            let _ = print_formals_map second_env
        in final_env second_env true)
        else (
            final_env second_env true)
