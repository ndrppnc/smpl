open Ast
open String

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
    Id(x)  -> x
  | Literal(l) -> string_of_literal l
  | Paren(a) -> "(" ^ convert_expr a ^ ")"
  | Binop(a, Add, b) -> (convert_expr a ^ "+" ^ convert_expr b)
  | Binop(a, Sub, b) -> (convert_expr a ^ "-" ^ convert_expr b)
  | Binop(a, Mult, b) -> (convert_expr a ^ "*" ^ convert_expr b)
  | Binop(a, Div, b) -> (convert_expr a ^ "/" ^ convert_expr b)
  | Binop(a, Mod, b) -> (convert_expr a ^ "%" ^ convert_expr b)
  | Binop(a, And, b) -> (convert_expr a ^ "&&" ^ convert_expr b)
  | Binop(a, Or, b) -> (convert_expr a ^ "||" ^ convert_expr b)
  | Binop(a, Equal, b) -> (convert_expr a ^ "==" ^ convert_expr b)
  | Binop(a, Neq, b) -> (convert_expr a ^ "!=" ^ convert_expr b)
  | Binop(a, Less, b) -> (convert_expr a ^ "<" ^ convert_expr b)
  | Binop(a, Leq, b) -> (convert_expr a ^ "<=" ^ convert_expr b)
  | Binop(a, Greater, b) -> (convert_expr a ^ ">" ^ convert_expr b)
  | Binop(a, Geq, b) -> (convert_expr a ^ ">=" ^ convert_expr b)
  | Assign(a, b) -> (a ^ "=" ^ convert_expr b)
  | Call(s1, al) -> if (s1 = "printf") then  
        	        ("printf" ^ "(" ^
          		((convert_expr (List.hd al)) ^ (List.fold_left (fun acc x -> acc ^ "," ^ (convert_expr x)) "" (List.tl al)))
          		^ ")")
                    else (s1 ^ "(" ^ String.concat "," 
        		(List.map (fun e -> convert_expr e ) al) ^ ")")
  | Noexpr -> ""

let rec tabs (n,s) =
	if n = 1 then
	  (1,s^"\t")
	else
	  tabs (n-1,s^"\t")

let build_args (a,n) =
	let list_size = List.length a in
	if list_size > 0 then
	  "void *args_thread_" ^ n ^ "[" ^ string_of_int list_size ^ "];\n" ^
	  (fst (List.fold_left (fun (acc,k) x -> (acc ^ 
	      			                  "args_thread_" ^ n ^ "[" ^ (string_of_int k) ^ "] = (void *)&" ^
				                  convert_expr x ^ ";\n",k+1)) ("",0) a))
	else
	  ""

let rec build_vars (n,s) =
	let var_type = "int" in
	let my_var = "\t" ^ var_type ^ " var" ^ (string_of_int (n-1)) ^ " = *(" ^ var_type ^ " *)arg_list[" ^ (string_of_int (n-1)) ^ "];\n" in
        if n = 1 then
          (1,my_var ^ s)
        else
          build_vars (n-1,my_var ^ s)

let build_thread_func (f,a,n) =
	let list_size = List.length a in
	"void *thread_" ^ n ^ "(void *args){\n" ^
        (if list_size > 0 then
	  "\tvoid **arg_list = (void **)args;\n" ^ 
          (snd (build_vars (list_size,"")))
        else
          "") ^
	"\t" ^ f ^ "(" ^
	(if list_size > 0 then
	  ("var0" ^ (fst (List.fold_left (fun (acc, k) x -> (acc ^ ",var" ^ (string_of_int k), k+1)) ("",1) (List.tl a))) ^ ");\n")
	else
	  "") ^
	"}\n\n"	

let build_pfor (n,i,l,c) =
	let num_threads = string_of_int n in
	let init = string_of_int i in
	let limit = string_of_int l in
	let k = string_of_int c in
	"int num_threads_" ^ k ^ " = " ^ num_threads ^ ";\n" ^
	"int pfor_init_" ^ k ^ " = " ^ init ^ ";\n" ^
  	"int pfor_limit_" ^ k ^ " = " ^ limit ^ ";\n\n" ^
  	"pthread_t pfor_threads_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	"int pfor_uppers_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	"int pfor_lowers_" ^ k ^ "[num_threads_" ^ k ^ "];\n" ^
	"int pfor_args_" ^ k ^ "[num_threads_" ^ k ^ "][2];\n" ^
	"int pfor_i_" ^ k ^ ";\n\n" ^
	"for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	"\tpfor_uppers_" ^ k ^ "[pfor_i_" ^ k ^ "] = pfor_init_" ^ k ^ "+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")/num_threads_" ^ k ^ ")*(pfor_i_" ^ k ^ "+1)+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")%num_threads_" ^ k ^ ");\n" ^
	"\tpfor_lowers_" ^ k ^ "[pfor_i_" ^ k ^ "] = pfor_init_" ^ k ^ "+((pfor_init_" ^ k ^ "+pfor_limit_" ^ k ^ ")/num_threads_" ^ k ^ ")*pfor_i_" ^ k ^ ";\n" ^
	"\tpfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "][0] = pfor_lowers_" ^ k ^ "[pfor_i_" ^ k ^ "];\n" ^
	"\tpfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "][1] = pfor_uppers_" ^ k ^ "[pfor_i_" ^ k ^ "];\n" ^
	"}\n\n" ^
	"for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	"\tpthread_create(&pfor_threads_" ^ k ^ "[pfor_i_" ^ k ^ "],NULL,pfor_func_" ^ k ^ ",(void *)&pfor_args_" ^ k ^ "[pfor_i_" ^ k ^ "]);\n" ^
	"}\n\n" ^
	"for(pfor_i_" ^ k ^ "=0;pfor_i_" ^ k ^ "<num_threads_" ^ k ^ ";pfor_i_" ^ k ^ "++){\n" ^
	"\tpthread_join(pfor_threads_" ^ k ^ "[pfor_i_" ^ k ^ "],NULL);\n" ^
  	"}\n"

let build_pfor_func (f,a,n) =
        let list_size = List.length a in
        "void *thread_" ^ n ^ "(void *args){\n" ^
	"\tint i;\n" ^
        "\tint *p = (int *)args;\n" ^
        "\tint lower = p[0];\n" ^
        "\tint upper = p[1];\n" ^
        (if list_size > 0 then
          "\tvoid **arg_list = (void **)args;\n" ^
	  (snd (build_vars (list_size,"")))
        else
          "") ^
        "\t" ^ f ^ "(" ^
        (if list_size > 0 then
          ("var0" ^ (fst (List.fold_left (fun (acc, k) x -> (acc ^ ",var" ^ (string_of_int k), k+1)) ("",1) (List.tl a))) ^ ");\n")
        else
          "") ^
	"\tpthread_exit(NULL);\n" ^
        "}\n\n"

(* my env stores couters for the
   number of locks, threads, and
   pfors -- in that order *)
let myenv = Array.make 3 0
let thread_funcs = Array.make 2 []
(*let pfor_funcs = Array.make 1 []*)

let rec convert_stmt = function
    Expr(e) -> convert_expr e ^ ";\n"
  | Declare(e) -> string_of_data_type e ^ ";\n"
  | DeclareAssign(a, b) -> string_of_data_type a ^ "=" ^ convert_expr b ^ ";\n"
  | Return(e) -> "return " ^ convert_expr e ^ ";\n"
  | Break(e) -> "break;\n"
  | Block(s) -> if (List.length s = 0) then "" else "{\n" ^ (List.fold_left (fun acc x -> acc ^ convert_stmt x) "" s) ^ "}\n"
  | If(e, s, n) -> let else_block = convert_stmt n in
 		  if (else_block = "") then "if(" ^ convert_expr e ^ ") " ^ convert_stmt s
 		  else "if(" ^ convert_expr e ^ ") " ^ convert_stmt s ^ "else " ^ else_block
  | For(i, c, u, s) -> "for(" ^ convert_expr i ^ "; " ^ convert_expr c ^ "; " ^ convert_expr u ^ "){" ^ convert_stmt s ^ "}\n"
  | While(e, s) -> "while(" ^ convert_expr e ^ ")" ^ convert_stmt s
  | Lock(s) -> myenv.(0) <- myenv.(0)+1; let lock = string_of_int myenv.(0) in
 		"pthread_mutex_lock(&m" ^ lock ^ ");\n" ^ convert_stmt s ^ "pthread_mutex_unlock(&m" ^ lock ^ ");\n"
  | Barrier(e) -> "int thread_counter;\nfor(thread_counter=0;thread_counter<" ^
		  string_of_int myenv.(1) ^ ";thread_counter++){\n" ^ "pthread_join(threads[thread_counter],NULL);\n" ^ "}\n"
  | Spawn(Call(id,args)) -> myenv.(1) <- myenv.(1)+1;
			(thread_funcs.(0) <- (build_thread_func (id,args,string_of_int (myenv.(1)-1)))::thread_funcs.(0));
			let thread = string_of_int (myenv.(1)-1) in 
			(build_args (args,thread)) ^ "pthread_create(&threads[" ^ thread ^ "],NULL,thread_" ^ string_of_int (myenv.(1)-1) ^ ",(void *)"
						   ^ (if List.length args > 0 then
							"args_thread_" ^ thread
						      else
							"NULL")
						   ^ ");\n" 
  | Pfor(t, i, b, s) -> myenv.(2) <- myenv.(2)+1;
			   build_pfor (int_of_string (convert_expr t),int_of_string (convert_expr i),int_of_string (convert_expr b),myenv.(2)-1)
  | Spawn(_) -> "" (* trying to spawn anything but a function call will be ignored *)

let rec locks (n,l) =
	if n = 0 then
	  (0, l)
	else
	  locks (n-1, ("pthread_mutex_t m" ^ string_of_int (n) ^ "=PTHREAD_MUTEX_INITIALIZER;\n") :: l)

let rec threads (n,l) =
        if n = 0 then
          (0, l)
        else
          threads (n-1, ("void *thread_" ^ string_of_int (n) ^ "(void *args);\n") :: l)

let string_of_fdecl (env,fdecl) =
  string_of_data_type fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    "){\n" ^
  String.concat "" (List.map convert_stmt fdecl.body) ^
  "}\n"

let convert_globals (env, globals) = 
    let data_type = fst globals in
    let var_id = id_of_data_type data_type in
    let (global_type, ref_count) = try Semantic_checker.StringMap.find 
    var_id env.Semantic_checker.globals_map with Not_found -> (var_id, 0)  in
    if(ref_count <> 0) then (
    match data_type with
          IntType(s) -> "int " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | FloatType(s) -> "float " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | BoolType(s) -> "bool " ^ s ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | CharType(s) -> "char " ^ s ^ " = " ^ "'" ^ string_of_literal (snd globals) ^ "';"
        | StrType(s) -> "char " ^ s ^ "[" ^
                        string_of_int (String.length (string_of_literal(snd globals)) - 1)
                        ^ "]" ^ " = " ^ string_of_literal (snd globals) ^ ";"
        | VoidType(s) -> ""
    ) else ("")

let rec append_env env = function
    [] -> []
    | hd::tl -> (env, hd)::append_env env tl

let generate_code (vars, funcs) env =
    let env_vars = append_env env vars in
    let env_funcs = append_env env funcs in
    "/* Code Generated from SMPL */\n" ^ includes ^
    String.concat "\n" (List.map convert_globals env_vars) ^ "\n" ^
    (List.fold_left (fun acc x -> acc ^ x) "" (snd (locks (myenv.(0), [])))) ^ "\n" ^
    (if myenv.(1) > 0 then "pthread_t threads[" ^ string_of_int myenv.(1) ^ "];\n\n" else "") ^
    (List.fold_left (fun acc x -> x ^ acc) "" thread_funcs.(0)) ^
    String.concat "\n" (List.map string_of_fdecl env_funcs)
