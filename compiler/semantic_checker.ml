open Ast
open Str

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
    formals_list    : (string * data_type) list; (* needed as map stores in alphabetical order, not in order of insertion*)
    visited_all     : bool;
    valid_syntax    : bool
  }


let print_map = false
let print_process_funcs = false

let validate_program (globalvars, funcs) =
    let print_string str_to_print =
        print_string ("/* " ^ str_to_print ^ " */\n")
    in let id_of_data_type = function
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

    in let string_of_data_type = function
        IntType(s) -> "integer " ^ s
        | FloatType(s) -> "float " ^ s
        | BoolType(s) -> "boolean " ^ s
        | CharType(s) -> "char " ^ s
        | StrType(s) -> "string " ^ s
        | VoidType(s) -> "void " ^ s 
    
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
        formals_list = [];
        visited_all = false;
        valid_syntax = false;
    }
    in let reserved_keyword keyword =
        if((keyword = "printf")||(keyword = "threads")||
        (keyword = "pthread_create")||(keyword = "pthread_join")
        ||(keyword = "pthread_mutex_t")||(keyword = "pthread_mutex_lock")
        ||(keyword = "pthread_mutex_unlock")||(keyword = "NULL")) then
            true
        else (
            let regex = Str.regexp ("\\(thread_.*\\)\\|\\(pfor_uppers_.*\\)"^
            "\\|\\(pfor_lowers_*\\)\\|\\(pfor_args_*\\)\\|\\(pfor_limit_*\\)"^
            "\\|\\(pfor_i_*\\)\\|\\(pfor_threads_*\\)\\|\\(pfor_init_*\\)"^
            "\\|\\(num_threads_*\\)\\|\\(lock_*\\)")
            in if(Str.string_match regex keyword 0) then
                true
            else
                false
        )
    in let supported_typecasting type_left type_right =
        if(type_left = "float" && (type_right =
            "integer"||type_right = "boolean"||type_right = "char")) then
            true
        else if(type_left = "integer" && (type_right =
                "boolean"||type_right = "char")) then
            true
        else if(type_left = "char" && type_right = "integer") then
            true
        else
            false


    (* For debugging purposes *)
    in let print_global_map env =
        StringMap.iter (fun dt_key (vartype, ref) -> print_string ("Global var: id: " ^ 
        dt_key ^ " type=" ^ vartype ^ " ref=" ^ (string_of_int ref)))
        env.globals_map
    in let print_function_map env = 
        StringMap.iter (fun dt_key (vartype, ref) -> print_string ("Function: id: " ^ 
        dt_key ^ " type=" ^ vartype ^ " ref=" ^ (string_of_int ref)))
        env.functions_map
    in let print_locals_map env = 
        VarMap.iter (fun (dt_func, dt_var) (var_type, var_ref) ->
            print_string ("Local Variable Func: " ^ dt_func ^ " var: " ^ dt_var ^ 
            " type: " ^ var_type ^ " ref= " ^ string_of_int var_ref)
        ) env.locals_map
    in let print_formals_map env =
        VarMap.iter (fun (dt_func, dt_arg) (arg_type) ->
            print_string ("Map Arguments Func: " ^ dt_func ^ " arg: " ^ dt_arg ^ " type: " ^
            arg_type)
        ) env.formals_map
    in let print_formals_list env =
        List.iter (fun (fname_id, arg) ->
            print_string ("List Arguments Func: " ^ fname_id ^ " arg: " ^
            (id_of_data_type arg) ^ " type: " ^ (get_data_type arg))
        ) env.formals_list
    in let print_unvisited_funcs env =
        List.iter (fun f -> print_string ("unvisited: "^f)) env.unvisited_funcs
    in let print_visited_funcs env = 
        List.iter (fun f -> print_string ("visited: "^f)) env.visited_funcs
    

    in let validate_global variable =
        if (get_data_type (fst variable) <> get_literal_type (snd variable)) then
            raise (Failure ("Global variable " ^ string_of_data_type (fst variable)
                ^ " is assigned a " ^ string_of_literal (snd variable)))
    
    in let add_local_var_to_map fname_id var env = 
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
        if(reserved_keyword id_arg) then
            raise (Failure ("Function:"^fname_id^": '"
                    ^ id_arg ^ "' is a reserved keyword. It cannot be " 
                    ^ "used as an argument name"))
        else(
            let found = VarMap.mem (fname_id, id_arg) env.formals_map in
            if(found = false) then (
                let new_env = { env with formals_list =
                    (fname_id, arg) :: env.formals_list }
                in
                { new_env with formals_map =
                    VarMap.add (fname_id, id_arg) (get_data_type arg)
                    new_env.formals_map }
            )
            else
                raise (Failure ("Function " ^ fname_id ^
                " contains more than one argument with the name " ^ id_arg))
        )
    
    in let get_func_formals_list fname_id env =
        List.fold_left (fun arg_list (dt_func, arg) ->
            if(dt_func = fname_id) then
                arg :: arg_list
            else
                arg_list
        ) [] env.formals_list



    in let rec process_expr fname_id env = function
          Literal(l) -> env
        | Paren(s) -> process_expr fname_id env s
        | Binop(e1, _, e2) -> 
                let new_env = process_expr fname_id env e1 in
                process_expr fname_id new_env e2
        | Not(e1) -> 
                process_expr fname_id env e1
        | Assign(id, idx) -> 
                let new_env = process_expr fname_id env id in
                process_expr fname_id new_env idx
        | Id(s)| Pp(s)| Mm(s) -> 
            let (var_type, ref_count) = try VarMap.find (fname_id, s) env.locals_map
                           with Not_found -> ("", 0) in
            if(var_type = "") then (
                let var_type  = try VarMap.find (fname_id, s) env.formals_map 
                                with Not_found -> "" in
                if(var_type = "") then (
                    let (var_type, ref_count) = try StringMap.find s env.globals_map
                                with Not_found -> ("", 0) in
                    if(var_type = "") then (
                        raise (Failure ("Variable '" ^ s ^ "' " ^ "in function '" ^ 
                            fname_id ^ "' is not defined"))
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
        | Noexpr -> env 

    in let update_env_map curr_env functions =
        List.fold_left (fun env argument -> 
                process_formals (id_of_data_type functions.fname) argument env)
            curr_env functions.formals 
    
    in let rec process_func_body fname env = function
        Block(stmts) ->
            List.fold_left (fun env_temp line ->
                process_func_body fname env_temp line) env stmts
        | Expr(exp) -> process_expr (id_of_data_type fname) env exp
        | Return(exp) -> process_expr (id_of_data_type fname) env exp
        | Break(_) -> env
        | Continue(_) -> env
        | Declare(decl) -> 
            add_local_var_to_map (id_of_data_type fname) decl env
        | DeclareAssign(decl, exp) -> 
            let new_env = add_local_var_to_map (id_of_data_type fname) decl env
            in process_expr (id_of_data_type fname) new_env exp
        | If (cond, then_clause, else_clause) ->
                let first_env = process_expr (id_of_data_type fname) env cond
                in let second_env = process_func_body fname first_env then_clause
                in process_func_body fname second_env else_clause
        | For (init, cond, inc, body) -> 
                let first_env = process_expr (id_of_data_type fname) env init
                in let second_env = process_expr (id_of_data_type fname) first_env cond
                in let third_env = process_expr (id_of_data_type fname) second_env inc
                in process_func_body fname third_env body
        | While (exp, body) ->
                let first_env = process_expr (id_of_data_type fname) env exp
                in process_func_body fname first_env body
        | Pfor(threads, counter, init, cond, body) ->
                let first_env = process_expr (id_of_data_type fname) env threads
                in let second_env = process_expr (id_of_data_type fname) first_env counter
                in let third_env = process_expr (id_of_data_type fname) second_env init
                in let fourth_env = process_expr (id_of_data_type fname) third_env cond
                in process_func_body fname fourth_env body
        | Spawn(exp) -> process_expr (id_of_data_type fname) env exp
        | Lock(stmt) -> process_func_body fname env stmt
        | Barrier(exp) -> env
    
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
            let _ = if(print_process_funcs) then (
                print_string ("process_code");
                print_unvisited_funcs new_env;
                print_visited_funcs new_env
            )
            in process_code new_env
        else
            new_env
    
    in let rec verify_expr fname_id env is_in_loop = function
        Literal(l) -> get_literal_type(l)
        | Paren(s) -> verify_expr fname_id env is_in_loop s
        | Binop(e1, o, e2) ->
                let type_e1 = verify_expr fname_id env is_in_loop e1
                in let type_e2 = verify_expr fname_id env is_in_loop e2
                in (match o with
                  Add|Sub|Mult|Div|Mod -> 
                        if((type_e1 = "string") || (type_e2 = "string")) then
                          raise (Failure ("Invalid operand to string in function " ^
                          fname_id))
                        else if((type_e1 = "void") || (type_e2 = "void")) then
                          raise (Failure ("Invalid operand to void in function " ^
                          fname_id))
                        else if((type_e1 = "float") || (type_e2 = "float")) then
                          "float"
                        else if((type_e1 = "integer") || (type_e2 = "integer")) then
                          "integer"
                        else if((type_e1 = "char") || (type_e2 = "char")) then
                          "char"
                        else if((type_e1 = "boolean") || (type_e2 = "boolean")) then
                          "boolean"
                        else
                          raise (Failure ("Invalid data type in function " ^
                          fname_id))
                | Equal|Neq|Greater|Geq|Less|Leq ->
                        if((type_e1 = "string") && (type_e2 <> "string")) then
                          raise (Failure ("Invalid operand to string in function " ^
                          fname_id))
                        else if((type_e2 = "string") && (type_e1 <> "string")) then
                            raise (Failure ("Invalid operand to string in function " 
                            ^ fname_id))
                        else if((type_e1 = "void") || (type_e2 = "void")) then
                          raise (Failure ("Invalid operand to void in function " ^
                        fname_id))
                        else
                            "boolean"
                | And|Or -> "boolean"
                )
        | Not(e) -> "boolean"
        | Assign(id, idx) ->
                let type_id = verify_expr fname_id env is_in_loop id
                in let type_right = verify_expr fname_id env is_in_loop idx
                in let id_name = (match id with
                    Id(s) -> s
                    | _ -> raise (Failure ("Unexpected error during Assign in function " ^
                            fname_id))
                )
                in if(type_id <> type_right) then (
                    if((supported_typecasting type_id type_right) = true) then
                        type_id
                    else
                        raise (Failure ("Type mismatch:" ^ type_id ^ " variable "
                            ^ id_name ^ " is assigned " ^ type_right ^ 
                            " in function " ^ fname_id)))
                else
                    type_id
        | Id(s) -> 
                let (var_type, _) = try VarMap.find (fname_id, s) env.locals_map
                                    with Not_found -> ("", 0) in
                if(var_type = "") then (
                    let var_type  = try VarMap.find (fname_id, s) env.formals_map 
                                    with Not_found -> "" in
                    if(var_type = "") then (
                        let (var_type, ref_count) = try StringMap.find s env.globals_map
                                                    with Not_found -> ("", 0) in
                        if(var_type = "") then (
                            raise (Failure ("Variable '" ^ s ^ "' " ^ "in function '" ^ 
                                            fname_id ^ "' is not defined"))
                         ) else
                            var_type
                    ) else
                        var_type
                ) else
                    var_type
        | Pp(s)|Mm(s) ->
                let (var_type, _) = try VarMap.find (fname_id, s) env.locals_map
                                    with Not_found -> ("", 0) in
                if(var_type = "") then (
                    let var_type  = try VarMap.find (fname_id, s) env.formals_map 
                                    with Not_found -> "" in
                    if(var_type = "") then (
                        let (var_type, ref_count) = try StringMap.find s env.globals_map
                                                    with Not_found -> ("", 0) in
                        if(var_type = "") then (
                            raise (Failure ("Variable '" ^ s ^ "' " ^ "in function '" ^ 
                                            fname_id ^ "' is not defined"))
                         ) else (
                             if(var_type <> "integer") then
                                 raise (Failure ("Function: '" ^ fname_id ^ "': Variable '" 
                                 ^ s ^ "' " ^ "is of type " ^ var_type ^
                                 ". However ++ and -- are supported only for integers"))
                             else
                                 var_type
                         )
                    ) else (
                        if(var_type <> "integer") then
                            raise (Failure ("Function: '" ^ fname_id ^ "': Variable '" 
                                 ^ s ^ "' " ^ "is of type " ^ var_type ^
                                 ". However ++ and -- are supported only for integers"))
                        else
                            var_type
                    )
                ) else (
                    if(var_type <> "integer") then
                        raise (Failure ("Function: '" ^ fname_id ^ "': Variable '" 
                                 ^ s ^ "' " ^ "is of type " ^ var_type ^
                                 ". However ++ and -- are supported only for integers"))
                    else
                        var_type
                )
        | Call(s1, al) ->
                let (var_type, _) = try StringMap.find s1 env.functions_map
                                            with Not_found -> ("", 0) in
                if(var_type = "" && (s1 <> "printf")) then
                    raise (Failure ("Function '" ^ s1 ^ "' is not defined"))
                else (
                    if(s1 = "printf") then
                        var_type
                    else (
                        let arg_list = get_func_formals_list s1 env
                        in let _ = try (List.iter2 (fun arg value -> 
                            let a_type = verify_expr fname_id env is_in_loop value in
                                if(get_data_type(arg) <> a_type) then (  
                                    raise(Failure ("Function "^fname_id^": call to:"^s1^
                                    " arg "^(id_of_data_type arg)^" is of type "^(get_data_type arg)^
                                    " but it is called with type "^a_type)))
                            )arg_list al)
                            with Invalid_argument(s) ->
                                (raise(Failure (fname_id^": call to:"^s1^
                                " number of arguments don't match")))
                        in var_type
                    )
                )

        | Noexpr -> ""
    
    in let rec verify_func_body fname env data_type is_in_loop = function
        Block(stmts) ->
            List.fold_left (fun new_data_type line ->
                verify_func_body fname env new_data_type is_in_loop line) data_type stmts
        | Expr(exp) -> verify_expr (id_of_data_type fname) env is_in_loop exp
        | Return(exp) -> 
                let return_type = verify_expr (id_of_data_type fname) env
                is_in_loop exp
                in let func_return_type = get_data_type (fname)
                in if((func_return_type = return_type) || (func_return_type =
                    "void" && return_type = "" )) then
                    ""
                else(
                    if(return_type = "") then
                        raise (Failure (" The return type of function " ^
                        id_of_data_type (fname) ^ " is " ^ func_return_type ^
                        " but it returns " ^ "void" ^ " type"))
                    else
                        raise (Failure (" The return type of function " ^
                        id_of_data_type (fname) ^ " is " ^ func_return_type ^
                        " but it returns " ^ return_type ^ " type"))
                )
        | Break(_) -> 
                if(is_in_loop) then
                        ""
                else
                    raise (Failure ("Function:'"^ id_of_data_type (fname) ^ 
                    "' break statement not within a loop"))
        | Continue(_) -> 
                if(is_in_loop) then
                        ""
                else
                    raise (Failure ("Function:'"^ id_of_data_type (fname) ^ 
                    "' continue statement not within a loop"))
        | Declare(decl) -> 
                (*TODO: Throw a warning if global or arg is
        defined with same name *)
                let func_name = id_of_data_type decl in  
                if(reserved_keyword func_name) then
                    raise (Failure ("Function:"^id_of_data_type(fname)^": '"
                    ^ func_name ^ "' is a reserved keyword. It cannot be " 
                    ^ "used as a variable name"))
                else
                    ""
        | DeclareAssign(decl, exp) ->
                (*TODO: Throw a warning if global or arg is
        defined with same name *)
                let func_name = id_of_data_type decl in  
                if(reserved_keyword func_name) then
                    raise (Failure ("Function:"^id_of_data_type(fname)^": '"
                    ^ func_name ^ "' is a reserved keyword. It cannot be " 
                    ^ "used as a variable name"))
                else(
                let assign_type = verify_expr (id_of_data_type fname) env
                is_in_loop exp
                in if(get_data_type (decl) = assign_type) then
                    ""
                else
                    if((supported_typecasting (get_data_type (decl)) assign_type)) then
                        ""
                    else
                        raise (Failure ("Local variable " ^ string_of_data_type (decl)
                            ^ " is assigned a " ^ assign_type ^ " in function " ^
                            id_of_data_type (fname)))
                )
        | If (cond, then_clause, else_clause) ->
                let _ = verify_expr (id_of_data_type fname) env is_in_loop cond
                in let _ = verify_func_body fname env "" is_in_loop then_clause
                in let _ = verify_func_body fname env "" is_in_loop else_clause
                in ""
        | For (init, cond, inc, body) -> 
                let _ = verify_expr (id_of_data_type fname) env is_in_loop init
                in let _ = verify_expr (id_of_data_type fname) env is_in_loop cond
                in let _ = verify_expr (id_of_data_type fname) env is_in_loop inc
                in let _ = verify_func_body fname env "" true body
                in ""
        | While (exp, body) ->
                let _ = verify_expr (id_of_data_type fname) env is_in_loop exp
                in let _ = verify_func_body fname env "" true body
                in ""
        | Pfor(threads, counter, init, cond, body) ->
                let _ = match counter with
                Id(s) -> 
                    let counter_type = verify_expr (id_of_data_type fname) env
                    is_in_loop counter in
                    if(counter_type <> "integer") then
                    raise (Failure ("Function: "^(id_of_data_type fname)^
                    "counter in pfor should be an integer variable ONLY."^
                    " Here it is "^ (counter_type)))
                | _ -> raise(Failure ("Function: "^(id_of_data_type fname)^
                        " counter in pfor should be an integer variable name ONLY"))
                in let data_type = 
                    verify_expr (id_of_data_type fname) env is_in_loop threads
                in if(data_type <> "integer") then
                    raise (Failure ("Function:"^(id_of_data_type fname)^
                        " First argument to pfor should be of type integer"))
                else (
                    let data_type = verify_expr (id_of_data_type fname) env
                    is_in_loop init
                    in if(data_type <> "integer") then
                        raise (Failure ("Function:"^(id_of_data_type fname)^
                        " Second argument to pfor should be of type integer"))
                    else (
                        let data_type = verify_expr (id_of_data_type fname) env
                        is_in_loop cond
                        in if(data_type <> "integer") then
                            raise (Failure ("Function:"^(id_of_data_type fname)^
                            " Third argument to pfor should be of type integer"))
                        else (
                            let _ = verify_func_body fname env "" true body
                            in ""
                        )
                    )
                )
        | Spawn(exp) ->
                let _ = match exp with
                Call(s1, al) -> if(s1 = "printf") then
                                    raise (Failure ("Function:'"^(id_of_data_type
                                    fname)^ "' printf is not allowed in spawn"))
                | _ -> raise (Failure ("Function:'"^(id_of_data_type fname)^
                        "' Invalid spawn syntax. spawn accepts" ^
                        " only function calls"))
                in let _ = verify_expr (id_of_data_type fname) env is_in_loop exp
                in ""
        | Lock(stmt) ->
                let _ = verify_func_body fname env "" is_in_loop stmt
                in ""
        | Barrier(exp) -> ""

    in let verify_function env str_dt fdecl =
        List.fold_left (fun str_data_type line ->
            verify_func_body fdecl.fname env str_data_type false line
            ) str_dt fdecl.body

    in let final_env curr_env valid =
        { curr_env with valid_syntax = valid }

    in let _ = List.iter validate_global globalvars
    in let first_env = List.fold_left (fun env func_list -> 
            update_env_map env func_list) global_env funcs 
    in let second_env = process_code first_env
    in let _ = List.fold_left (fun str_dt func -> 
        let func_name = id_of_data_type func.fname 
        in if(reserved_keyword func_name) then
            raise (Failure ("Function:"^func_name^": '"^func_name^
                    "' is a reserved keyword. It cannot be used as a"^
                    " function name"))
        else (
            let (var_type, ref_count) = try StringMap.find func_name second_env.functions_map
            with Not_found -> ("", 0) in
            if(ref_count <> 0) then
                verify_function second_env str_dt func
            else
                ""
        )
        ) "" funcs
    in if (print_map) then (
        let _ = print_global_map second_env in
        let _ = print_function_map second_env in
        let _ = print_locals_map second_env in
        let _ = print_formals_map second_env in
        let _ = print_formals_list second_env
        in final_env second_env true)
    else (
        final_env second_env true)
