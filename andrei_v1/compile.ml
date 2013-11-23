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
  | Call(s1, al) -> if (s1 = "print") then  
                ("printf" ^ "(" ^ String.concat "," 
        (List.map (fun e -> convert_expr e ) al) ^ ")")
                    else (s1 ^ "(" ^ String.concat "," 
        (List.map (fun e -> convert_expr e ) al) ^ ")")
  | _ -> ""

(*
let rec print_tabs n = function
   _ -> if(n != 0) then "\t" ^ print_tabs n-1 
*)

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
  | For(i, c, u, s) -> "for(" ^ convert_expr i ^ "; " ^ convert_expr c ^ "; " ^ convert_expr u ^ ") " ^ convert_stmt s
  | While(e, s) -> "while(" ^ convert_expr e ^ ")" ^ convert_stmt s
  | Lock(s) -> let lock = string_of_int 0 in
 		"{\n" ^ "pthread_mutex_lock(&m" ^ lock ^ ");\n" ^ convert_stmt s ^ "pthread_mutex_unlock(&m" ^ lock ^ ");\n}\n"
  | Barrier(e) -> "for(i=0;i<num_threads;i++){\n" ^ "pthread_join(threads[i],NULL);\n" ^ "}\n"
  | Spawn(Call(id,args)) -> let threads = string_of_int 0 in 
			"pthread_create(&threads[" ^ threads ^ "],NULL," ^ id ^ (List.fold_left (fun acc x -> acc ^ "," ^ convert_expr x) "" args) ^ ");\n"
(*| Pfor(t, i, c, u, s) -> "for(i=0; i<" ^ t ^ "; i++) " ^ convert_stmt Spawn(Call(func_id,func_args)) ^
			"\n\n" ^ convert_stmt Barrier() *)
  | _ -> ""

let locks = ""

let threads = ""

let string_of_fdecl fdecl =
  string_of_data_type fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    "){\n" ^
  String.concat "" (List.map convert_stmt fdecl.body) ^
  "}\n"

let convert_globals globals = 
    let data_type = fst globals in
    match data_type with
          IntType(s) -> "int " ^ s ^ " = " ^ string_of_literal (snd globals)
        | FloatType(s) -> "float " ^ s ^ " = " ^ string_of_literal (snd globals)
        | BoolType(s) -> "bool " ^ s ^ " = " ^ string_of_literal (snd globals)
        | CharType(s) -> "char " ^ s ^ " = " ^ "'" ^ string_of_literal (snd globals) ^ "'"
        | StrType(s) -> "char " ^ s ^ "[" ^ 
                        string_of_int (String.length (string_of_literal(snd globals)) - 1) 
                        ^ "]" ^ " = " ^ string_of_literal (snd globals)
        | VoidType(s) -> ""

let generate_code (vars, funcs) =
    "/* Code Generated from SMPL */\n" ^ includes ^
    locks ^ threads ^
    String.concat ";\n" (List.map convert_globals vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)
