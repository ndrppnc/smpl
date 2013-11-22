type action = DumpAst | CodeGen

let _ =
    if Array.length Sys.argv <> 3 then (
        let result = "NAME: \n\tsmpl - Compiler for generating C code for SiMPLe" ^
        " Programming Language\nARGUMENTS:\n\t -a <code-file> Print the parse" ^
        " results of the SMPL code in <code-file>\n\t -c <code-file> Generate the C" ^
        " code equivalent to the SMPL code in <code-file>\nAUTHORS:\n\tWritten" ^
        " by Ajay Siva Santosh, Andrei Papancea, Devashi Tandon\n\n" in
        print_string result
    ) else (
        let action = List.assoc Sys.argv.(1) [ ("-a", DumpAst); ("-c", CodeGen)] in
    let src_file = Sys.argv.(2) in  
    let lexbuf = Lexing.from_channel (open_in src_file) in
    let expr = Parser.program Scanner.token lexbuf in
    if (Semantic_checker.validate_program expr = ref true) then
        match action with
        DumpAst -> 
            let result = Printast.parse_program expr in
            print_string result
        | CodeGen -> 
            let result = Compile.generate_code expr in
            print_string result
                      
    )
