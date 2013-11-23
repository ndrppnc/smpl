type action = DumpAst | CodeGen

let process_program lexbuf action = 
    let expr = Parser.program Scanner.token lexbuf in
    if (Semantic_checker.validate_program expr = ref true) then
        match action with
        DumpAst -> 
            let result = Printast.parse_program expr in
            print_string result
        | CodeGen -> 
            let result = Compile.generate_code expr in
            print_string result

let _ =
    if Array.length Sys.argv <> 3 then (
        let result = "NAME: \n\tsmpl - Compiler for generating C code for SiMPLe" ^
        " Programming Language\nARGUMENTS:\n\t -a code-input Print the parse" ^
        " results of the input SMPL code \n\t -c code-input Generate the C" ^
        " code equivalent to the input SMPL code\n code-input can be one of the" ^
        " following:\n\t -t Take input from terminal\n\t <file-name> Take input" ^
        " from file\nEXAMPLES:\n\t./smplc -a -t : Take code input from" ^
        " terminal and print the parse results.\n\t./smplc -c hello.smpl >" ^
        " hello.c: Compile the SMPL code in hello.smpl and write the generated C" ^ 
        " code to hello.c\n\nAUTHORS:\n\tWritten" ^
        " by Ajay Siva Santosh, Andrei Papancea, Devashi Tandon\n\n" in
        print_string result
    ) else (
        let action = List.assoc Sys.argv.(1) [ ("-a", DumpAst); ("-c", CodeGen)] in
    let src_file = Sys.argv.(2) in  
    if(src_file = "-t") then
        let lexbuf = Lexing.from_channel stdin in
        process_program lexbuf action
    else
        let lexbuf = Lexing.from_channel (open_in src_file) in 
        process_program lexbuf action
    )
