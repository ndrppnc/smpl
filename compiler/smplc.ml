open Ast

let _ =
 let src_file = Sys.argv.(1) in  
  let lexbuf = Lexing.from_channel (open_in src_file) in
  let expr = Parser.program Scanner.token lexbuf in
  let result = Ast.string_of_program expr in
  print_string result
