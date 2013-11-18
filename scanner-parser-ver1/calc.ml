open Ast

let input_arr = Array.make 10 0;;

let rec eval = function 
    Literal(x) -> print_endline ("Literal " ^ string_of_int x);0;
  | Float(x) -> print_endline ("Float " ^ string_of_float x);0;
  | Char(x) -> print_endline ("Char " ^ Char.escaped x);0;
  | String(x) -> print_endline ("String " ^ x);0;
  | Boolean(x) -> print_endline (string_of_bool x); 0;
  | Id(x) -> print_endline ("Id: " ^ x);0;
  | Call(x, y) -> print_endline ("Call: " ^ x);0;
  | Assign(l1, r1) -> let r2 = eval r1 in print_endline ("Assign: l1 " ^ l1 ^ " " ^ string_of_int r2); 0;
  | Noexpr -> print_endline ("Noexpr");0
  | Binop(e1, op, e2) ->
      let v1 = eval e1 and v2 = eval e2 in
      match op with
        Add -> print_endline("Add " ^ string_of_int v1 ^ ", " ^ string_of_int v2); v1 + v2
      | Sub -> print_endline("Sub " ^ string_of_int v1 ^ ", " ^ string_of_int v2);v1 - v2
      | Mult -> print_endline("Mult " ^ string_of_int v1 ^ ", " ^ string_of_int v2);v1 * v2
      | Div -> print_endline("Div " ^ string_of_int v1 ^ ", " ^ string_of_int v2);v1 / v2
      | Mod -> print_endline("Mod " ^ string_of_int v1 ^ ", " ^ string_of_int v2);v1 mod v2
      | Equal -> print_endline("Equal " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Neq -> print_endline("Neq " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Less -> print_endline("Less " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Leq -> print_endline("Leq " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Greater -> print_endline("Greater " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Geq -> print_endline("Geq " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | And -> print_endline("And " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0
      | Or -> print_endline("Or " ^ string_of_int v1 ^ ", " ^ string_of_int v2);0

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
