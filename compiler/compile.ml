open Ast
open String

module VarMap = Map.Make(struct
                type t = string * string
                let compare x y = Pervasives.compare x y
        end)

let includes = 
    "#include <stdio.h>\n" ^
    "#include <stdbool.h>\n" ^
    "#include <stdlib.h>\n" ^
    "#include <string.h>\n" ^
    "#include <pthread.h>\n\n"

let rec string_of_data_type = function
    IntType(s) -> "int " ^ s
  | FloatType(s) -> "float " ^ s
  | BoolType(s) -> "bool " ^ s
  | CharType(s) -> "char " ^ s
  | StrType(s) -> "char * " ^ s
  | VoidType(s) -> "void " ^ s 

let convert_data_type = function
    "integer" -> "int"
  | "float" -> "float"
  | "boolean" -> "bool"
  | "char" -> "char"
  | "string" -> "char *"
  | _ -> ""

let get_data_type = function
    IntType(s) -> "int"
  | FloatType(s) -> "float"
  | BoolType(s) -> "bool"
  | CharType(s) -> "char"
  | StrType(s) -> "char *"
  | VoidType(s) -> "void"

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
  | Char(c) -> "'" ^ Char.escaped c ^ "'"
  | String(s) -> "\"" ^ s ^ "\""

let type_of_literal = function
    Integer(i) -> "int"
  | Float(f) -> "float"
  | Boolean(b) -> "bool"
  | Char(c) -> "char"
  | String(s) -> "char *"

let is_literal = function
    Literal(l) -> true
  | _ -> false

let get_literal_type = function
    Literal(l) -> type_of_literal(l)
  | _ -> ""

let is_assignment = function
    Assign(a, b) -> true
  | _ -> false

(* keep track of assignments in pfor *)
let assignments_in_pfor = ref ""
let globals_in_pfor_assignments = ref []
let globals_init_values = ref []

let generate_code (vars, funcs) env =
    let rec append_env env = function
    [] -> []
    | hd::tl -> (env, hd)::append_env env tl
    in let env_vars = append_env env vars in
    let global_inits =
	ignore (List.map (fun (env,globals) ->
     	  let data_type = fst globals in
    	  let var_id = id_of_data_type data_type in
    	  let (global_type, ref_count) = try Semantic_checker.StringMap.find
    	  var_id env.Semantic_checker.globals_map with Not_found -> (var_id, 0)  in
    	  if(ref_count <> 0) then (
            match data_type with
            IntType(s) -> globals_init_values := (s,string_of_literal (snd globals))::!globals_init_values; ""
          | FloatType(s) -> globals_init_values := (s,string_of_literal (snd globals))::!globals_init_values; ""
          | BoolType(s) -> globals_init_values := (s,string_of_literal (snd globals))::!globals_init_values; ""
          | CharType(s) -> globals_init_values := (s, string_of_literal (snd globals))::!globals_init_values; ""
          | StrType(s) -> globals_init_values := (s,string_of_literal (snd globals))::!globals_init_values; ""
          | VoidType(s) -> ("")
          ) else ("")
	) env_vars); ""
    in let rec convert_expr env is_pfor is_replace myA myB = function
    Id(x)  -> if is_replace && x = myA then
                   myB
              else
	        if is_pfor then (
	          let (var_type, ref_count) = try Semantic_checker.StringMap.find x env.Semantic_checker.globals_map
                                	    with Not_found -> ("", 0) in
                  if(var_type = "") then x else (
			(if not (List.mem_assoc x !globals_in_pfor_assignments) then (
				globals_in_pfor_assignments := (x,var_type) :: !globals_in_pfor_assignments
			 ) else ());
			"temp_" ^ x
		  )	
	        ) else x
  | Literal(l) -> string_of_literal l
  | Paren(a) -> "(" ^ convert_expr env is_pfor is_replace myA myB a ^ ")"
  | Binop(a, Add, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "+" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Sub, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "-" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Mult, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "*" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Div, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "/" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Mod, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "%" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, And, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "&&" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Or, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "||" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Equal, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "==" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Neq, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "!=" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Less, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "<" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Leq, b) -> (convert_expr env is_pfor is_replace myA myB a ^ "<=" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Greater, b) -> (convert_expr env is_pfor is_replace myA myB a ^ ">" ^ convert_expr env is_pfor is_replace myA myB b)
  | Binop(a, Geq, b) -> (convert_expr env is_pfor is_replace myA myB a ^ ">=" ^ convert_expr env is_pfor is_replace myA myB b)
  | Not(e) -> "!" ^ convert_expr env is_pfor is_replace myA myB e
  | Pp(id) -> (id ^ "++")
  | Mm(id) -> (id ^ "--")
  | Assign(a, b) -> if is_pfor then (
			assignments_in_pfor :=	!assignments_in_pfor ^
						"    pthread_mutex_lock(&pfor_global_lock);\n" ^
						"    " ^ (convert_expr env false is_replace myA myB a ^ "=" ^
					                 (convert_expr env false true "i" (convert_expr env is_pfor false myA myB a) b) ^ ";\n") ^
						"    " ^ "pthread_mutex_unlock(&pfor_global_lock);\n";
			(convert_expr env is_pfor is_replace myA myB a ^ "=" ^ convert_expr env is_pfor false myA myB b)
		    ) else
			(convert_expr env is_pfor is_replace myA myB a ^ "=" ^ convert_expr env is_pfor is_replace myA myB b)
  | Call(s1, al) -> if (s1 = "printf") then  
        	        ("printf" ^ "(" ^
          		((convert_expr env is_pfor is_replace myA myB (List.hd al)) ^
			 (List.fold_left (fun acc x -> acc ^ "," ^ (convert_expr env is_pfor is_replace myA myB x)) "" (List.tl al)))
          		^ ")")
                    else (s1 ^ "(" ^ String.concat "," 
        		(List.map (fun e -> convert_expr env is_pfor is_replace myA myB e ) al) ^ ")")
  | Noexpr -> ""

  in let rec indent n =
	if n = 1 then
	  "    "
	else
	  "    "^indent(n-1)

  in let build_args (a,n,num_indent,env,is_pfor) =
	let list_size = List.length a in
	let lit_counter = Array.make 1 0 in
	if list_size > 0 then
	  indent num_indent ^ "void *args_thread_" ^ n ^ "[" ^ string_of_int list_size ^ "];\n" ^
	  (fst (List.fold_left (fun (acc,k) x -> (acc ^
						 (if is_literal(x) then
                                                  	indent num_indent ^ get_literal_type x ^ " arg_lit_" ^ n ^ "_"
							^ (string_of_int k) ^ " = " ^ convert_expr env is_pfor false "x" "y" x ^ ";\n"
						  else
							""),k+1)) ("",0) a)) ^
	  (fst (List.fold_left (fun (acc,k) x -> (acc ^ 
	      			                  indent num_indent ^ "args_thread_" ^ n ^ "[" ^ (string_of_int k) ^ "] = (void *)&" ^
				                  (if is_literal(x) then		
							(lit_counter.(0) <- lit_counter.(0)+1;
							"arg_lit_" ^ n ^ "_" ^ (string_of_int (lit_counter.(0)-1)))
						   else
							convert_expr env is_pfor false "x" "y" x)
						   ^ ";\n",k+1)) ("",0) a))
	else
	  ""

    in let get_func_formals_list fname_id env =
        List.fold_left (fun arg_list (dt_func, arg) ->
            if(dt_func = fname_id) then
                arg :: arg_list
            else
                arg_list
        ) [] env.Semantic_checker.formals_list

    in let get_func_locals_list fname_id env = 
        Semantic_checker.VarMap.fold (fun (my_fun, var_id) (var_type, ref) var_list ->
            if(my_fun = fname_id) then (
                let (var_type, ref_count) = try Semantic_checker.VarMap.find
                (fname_id,var_id) env.Semantic_checker.locals_map with Not_found -> (var_id, 0) in
                if(ref_count <> 0) then (
                   (var_id,var_type) :: var_list
                ) else var_list
            ) else
                var_list
        ) env.Semantic_checker.locals_map []

    in let rec build_vars (f,n,counter,s,k,env) =
	let test = Array.make 1 0 in
	let data_type = Array.make 1 "" in
	let var_type = List.iter (fun arg ->
	    if counter-n = test.(0) then (
                data_type.(0) <- (get_data_type arg);
                test.(0) <- test.(0)+1
            ) else
                test.(0) <- test.(0)+1
        ) (List.rev (get_func_formals_list f env)); data_type.(0) in
	let my_var = (indent 1) ^ var_type ^ " var" ^ (string_of_int (n-1)) ^ " = *(" ^ var_type ^ " *)arg_list[" ^ (string_of_int (n-1)) ^ "];\n" in
        if n = 1 then
          (1,my_var ^ s)
        else
          build_vars (f,n-1,counter,my_var ^ s,k+1,env)

    in let build_thread_func (f,a,n,env) =
	let list_size = List.length a in
	"void *thread_" ^ n ^ "(void *args){\n" ^
        (if list_size > 0 then
	  (indent 1) ^"void **arg_list = (void **)args;\n" ^ 
          (snd (build_vars (f,list_size,list_size,"",0,env)))
        else
          "") ^
	(indent 1) ^ f ^ "(" ^
	(if list_size > 0 then
	  ("var0" ^ (fst (List.fold_left (fun (acc, k) x -> (acc ^ ",var" ^ (string_of_int k), k+1)) ("",1) (List.tl a))) ^ ");\n")
	else
            ");\n") ^
	"}\n\n"	

    in let build_pfor (f,n,i,l,c,remove,num_indent,env) =
	let num_threads = n in
	let init = i in
	let limit = l in
	let k = string_of_int c in
	let locals_list = List.rev (get_func_locals_list f env) in
	indent num_indent ^ "int num_threads_" ^ k ^ " = " ^ num_threads ^ ";\n" ^
	indent num_indent ^ "int pfor_init_" ^ k ^ " = " ^ init ^ ";\n" ^
  	indent num_indent ^ "int pfor_limit_" ^ k ^ " = " ^ limit ^ ";\n\n" ^
  	indent num_indent ^ "pthread_t pfor_threads_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	indent num_indent ^ "int pfor_uppers_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	indent num_indent ^ "int pfor_lowers_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	indent num_indent ^ "void *pfor_args_" ^ k ^ "[num_threads_" ^ k ^ "][3];\n" ^
	indent num_indent ^ "void *pfor_local_vars_" ^ k ^ "[" ^ string_of_int (List.length locals_list) ^ "];\n" ^
	(fst (List.fold_left (fun (acc, count) (var_id, var_type) ->
	    if not (List.mem var_id remove) then (
	      (acc ^ indent num_indent ^ "pfor_local_vars_" ^ k ^ "[" ^ string_of_int count ^ "] = (void *)&" ^ var_id ^ ";\n", count+1)
            ) else
	      (acc, count)
	) ("",0) locals_list)) ^
	indent num_indent ^ "int pfor_i_" ^ k ^ ";\n\n" ^
	indent num_indent ^ "for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	indent (num_indent+1) ^ "pfor_uppers_" ^ k ^ "[pfor_i_" ^ k ^ "] = pfor_init_" ^ k ^ "+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")/num_threads_" ^ k ^ ")*(pfor_i_" ^ k ^ "+1)+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")%num_threads_" ^ k ^ ");\n" ^
	indent (num_indent+1) ^ "pfor_lowers_" ^ k ^ "[pfor_i_" ^ k ^ "] = pfor_init_" ^ k ^ "+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")/num_threads_" ^ k ^ ")*pfor_i_" ^ k ^ ";\n" ^
	indent (num_indent+1) ^ "pfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "][0] = (void *)&pfor_lowers_" ^ k ^ "[pfor_i_" ^ k ^ "];\n" ^
	indent (num_indent+1) ^ "pfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "][1] = (void *)&pfor_uppers_" ^ k ^ "[pfor_i_" ^ k ^ "];\n" ^
	indent (num_indent+1) ^ "pfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "][2] = (void *)&pfor_local_vars_" ^ k ^ ";\n" ^
	indent num_indent ^ "}\n\n" ^
	indent num_indent ^ "for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	indent (num_indent+1) ^ "pthread_create(&pfor_threads_" ^ k ^ "[pfor_i_" ^ k ^ "],NULL,thread_" ^ k ^ ",(void *)&pfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "]);\n" ^
	indent num_indent ^ "}\n\n" ^
	indent num_indent ^ "for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	indent (num_indent+1) ^ "pthread_join(pfor_threads_" ^ k ^ "[pfor_i_" ^ k ^ "],NULL);\n" ^
  	indent num_indent ^ "}\n"

(* myenv stores counters for the
   number of locks, threads, and
   pfors -- in that order *)
    in let myenv = Array.make 3 0
    in let thread_funcs = Array.make 2 []
    in let remove = Array.make 1 []

    in let rec convert_stmt num_indent env currf is_pfor is_replace myA myB = function
    Expr(e) -> indent num_indent ^ convert_expr env is_pfor is_replace myA myB e ^ ";\n"
  | Declare(e) -> let data_type = e in
        	  let var_id = id_of_data_type data_type in
        	  let (var_type, ref_count) = try Semantic_checker.VarMap.find
        	  (currf,var_id) env.Semantic_checker.locals_map with Not_found -> (var_id, 0) in
        	  if(ref_count <> 0) then (
		     if is_pfor then (
			remove.(0) <- var_id::remove.(0); ()
		     ) else ();
	   	     indent num_indent ^ string_of_data_type e ^ ";\n"
        	  ) else ("") 
  | DeclareAssign(a, b) -> let data_type = a in
                  	   let var_id = id_of_data_type data_type in
                  	   let (var_type, ref_count) = try Semantic_checker.VarMap.find
                  	   (currf,var_id) env.Semantic_checker.locals_map with Not_found -> (var_id, 0) in
                  	   if(ref_count <> 0) then (
                     		if is_pfor then (
                       		   remove.(0) <- var_id::remove.(0); ()
                     		) else ();
				indent num_indent ^ string_of_data_type a ^ "=" ^ convert_expr env is_pfor is_replace myA myB b ^ ";\n"
                  	   ) else ("")
  | Return(e) -> indent num_indent ^ "return " ^ convert_expr env is_pfor is_replace myA myB e ^ ";\n"
  | Break(e) -> indent num_indent ^ "break;\n"
  | Continue(e) -> indent num_indent ^ "continue;\n"
  | Block(s) -> if (List.length s = 0) then "" else "{\n" ^ (List.fold_left (fun acc x -> 
          acc ^ convert_stmt (num_indent + 1) env currf is_pfor is_replace myA myB x) "" s) ^ (indent (num_indent)) ^ "}\n"
  | If(e, s, n) -> let else_block = convert_stmt num_indent env currf is_pfor is_replace myA myB n in
 		  if (else_block = "") then (indent num_indent) ^ "if(" ^ convert_expr env is_pfor is_replace myA myB e ^ ")" ^
                  convert_stmt num_indent env currf is_pfor is_replace myA myB s
 		  else (indent num_indent) ^ "if(" ^ convert_expr env is_pfor is_replace myA myB e ^ ")" ^
                  convert_stmt num_indent env currf is_pfor is_replace myA myB s ^(indent num_indent) ^ "else" ^ else_block
  | For(i, c, u, s) -> (indent num_indent) ^ "for(" ^ convert_expr env is_pfor is_replace myA myB i ^ "; " ^ convert_expr env is_pfor is_replace myA myB c ^ "; " ^
  			 			      convert_expr env is_pfor is_replace myA myB u ^ ")" ^
					     convert_stmt (num_indent) env currf is_pfor is_replace myA myB s ^ "\n"
  | While(e, s) -> (indent num_indent) ^ "while(" ^ convert_expr env is_pfor is_replace myA myB e ^ ")"
		   ^ convert_stmt (num_indent) env currf is_pfor is_replace myA myB s
  | Lock(s) -> myenv.(0) <- myenv.(0)+1; let lock = string_of_int myenv.(0) in
               (indent num_indent) ^ "pthread_mutex_lock(&m" ^ lock ^ ");\n" ^ convert_stmt num_indent env currf is_pfor is_replace myA myB s ^ 
               "pthread_mutex_unlock(&m" ^ lock ^ ");\n"
  | Barrier(e) ->(indent num_indent) ^ "int thread_counter;\n"^(indent num_indent) ^ "for(thread_counter=0;thread_counter<" ^
		  string_of_int myenv.(1) ^ ";thread_counter++){\n" ^(indent (num_indent+1)) ^ 
                  "pthread_join(threads[thread_counter],NULL);\n" ^(indent num_indent) ^ "}\n"
  | Spawn(Call(id,args)) -> myenv.(1) <- myenv.(1)+1;
			(thread_funcs.(0) <- (build_thread_func (id,args,string_of_int (myenv.(1)-1),env))::thread_funcs.(0));
			let thread = string_of_int (myenv.(1)-1) in 
			(build_args (args,thread,num_indent,env,is_pfor)) ^
			indent num_indent ^ "pthread_create(&threads[" ^ thread ^ "],NULL,thread_" ^ string_of_int (myenv.(1)-1) ^ ",(void *)"
						   ^ (if List.length args > 0 then
							"args_thread_" ^ thread
						      else
							"NULL")
						   ^ ");\n" 
  | Pfor(t, k, i, b, s) -> let build_pfor_func (stmt,n,env) =
				assignments_in_pfor := "";
				globals_in_pfor_assignments := [];
				let counter = convert_expr env is_pfor is_replace myA myB k in
				(indent 1) ^ "pthread_mutex_t pfor_global_lock=PTHREAD_MUTEX_INITIALIZER;\n" ^
        			(indent 1) ^ "void **arguments = (void **)args;\n" ^
 				(indent 1) ^ "int lower = *((int *)arguments[0]);\n" ^
				(indent 1) ^ "int upper = *((int *)arguments[1]);" ^
        			(let locals = (List.fold_left (fun (acc, count) (var_id, var_type) ->
            				if not (List.mem var_id remove.(0)) then (
					(acc ^ indent num_indent ^ (convert_data_type var_type) ^
					 " " ^ var_id ^ " = " ^ "*((" ^ convert_data_type var_type ^ " *)thread_0_local_vars[" ^ string_of_int count ^ "]);\n", count+1)
					) else (acc, count)
        			) ("",0) (List.rev (get_func_locals_list currf env))) in
				if snd locals > 0 then (
					(indent 1) ^ "void **thread_0_local_vars = (void **)arguments[2];\n" ^
					(fst locals)
				) else "") ^
				(indent 1) ^"for(" ^ counter ^ "=lower; " ^ counter ^ "<upper; " ^ counter ^ "++)" ^
				convert_stmt 1 env currf true is_replace myA myB stmt in
			let get_global_init var_id = 
				List.fold_left (fun acc (id,value) -> if var_id = id then value else acc) "" !globals_init_values in
			myenv.(1) <- myenv.(1)+1;
			let pfor_func = (build_pfor_func (s,string_of_int (myenv.(1)-1),env)) in
			let globals_in_assignments = List.fold_left (fun acc (var_id,var_type) ->
									acc ^ "    " ^ convert_data_type var_type ^ " temp_" ^ var_id ^ " = " ^
									get_global_init var_id ^ ";\n"
								     ) "" !globals_in_pfor_assignments in
			(thread_funcs.(0) <- ("void *thread_" ^ string_of_int (myenv.(1)-1) ^ "(void *args){\n" ^
					      globals_in_assignments ^
					      pfor_func ^ !assignments_in_pfor ^
                                	     (indent 1) ^"pthread_exit(NULL);\n" ^
                                	     "}\n\n")::thread_funcs.(0));
			build_pfor (currf,(convert_expr env is_pfor is_replace myA myB t),
					  (convert_expr env is_pfor is_replace myA myB i),
					  (convert_expr env is_pfor is_replace myA myB b),
				    myenv.(1)-1,remove.(0),num_indent,env)
  | Spawn(_) -> "" (* trying to spawn anything but a function call will be ignored *)

  in let rec locks (n,l) =
	if n = 0 then
	  (0, l)
	else
	  locks (n-1, ("pthread_mutex_t m" ^ string_of_int (n) ^ "=PTHREAD_MUTEX_INITIALIZER;\n") :: l)

  in let rec threads (n,l) =
        if n = 0 then
          (0, l)
        else
          threads (n-1, ("void *thread_" ^ string_of_int (n-1) ^ "(void *args);\n") :: l)

  in let convert_globals (env, globals) = 
    let data_type = fst globals in
    let var_id = id_of_data_type data_type in
    let (global_type, ref_count) = try Semantic_checker.StringMap.find 
    var_id env.Semantic_checker.globals_map with Not_found -> (var_id, 0)  in
    if(ref_count <> 0) then (
	match data_type with
          IntType(s) -> "int " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | FloatType(s) -> "float " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | BoolType(s) -> "bool " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | CharType(s) -> "char " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | StrType(s) -> "char " ^ s ^ "[" ^
                        string_of_int (String.length (string_of_literal(snd globals)) - 1)
                        ^ "]" ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | VoidType(s) -> ""
    ) else ("")

  in let string_of_fdecl (env,fdecl) =
    let data_type = fdecl.fname in
    let var_id = id_of_data_type data_type in
    let (func_type, ref_count) = try Semantic_checker.StringMap.find
    var_id env.Semantic_checker.functions_map with Not_found -> (var_id, 0)  in
    if(ref_count <> 0) then (
        string_of_data_type fdecl.fname ^ "(" ^
        String.concat "," (List.map string_of_data_type fdecl.formals) ^
        "){\n" ^
        String.concat "" (List.map (convert_stmt 1 env (id_of_data_type fdecl.fname) false false "x" "y") fdecl.body) ^
        "}\n"
    ) else ("") 

  in let rec append_env env = function
    [] -> []
    | hd::tl -> (env, hd)::append_env env tl

    in let env_vars = append_env env vars in
    let env_funcs = append_env env funcs in
    "/* Code Generated from SMPL */\n" ^ includes ^
    String.concat "\n" (List.map convert_globals env_vars) ^ global_inits ^ "\n\n" ^
    String.concat "\n" (List.map (fun (tenv,fdecl) ->
        let name = string_of_data_type fdecl.fname in
        if name <> "int main" then (
           let data_type = fdecl.fname in
           let var_id = id_of_data_type data_type in
           let (func_type, ref_count) = try Semantic_checker.StringMap.find
           var_id env.Semantic_checker.functions_map with Not_found -> (var_id, 0)  in
           if(ref_count <> 0) then (
                string_of_data_type fdecl.fname ^ "(" ^ String.concat "," (List.map string_of_data_type fdecl.formals) ^ ");"
           ) else ("")
        ) else
           ""
    ) env_funcs) ^
    (List.fold_left (fun acc x -> acc ^ x) "" (snd (locks (myenv.(0), [])))) ^ "\n" ^
    (List.fold_left (fun acc x -> acc ^ x) "" (snd (threads (myenv.(1), [])))) ^ "\n" ^
    (if myenv.(1) > 0 then "pthread_t threads[" ^ string_of_int myenv.(1) ^ "];\n\n" else "") ^
    (List.fold_left (fun acc x -> x ^ acc) "" thread_funcs.(0) ^
     String.concat "\n" (List.map string_of_fdecl env_funcs))
