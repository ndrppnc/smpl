type action = DumpAst | CodeGen | SyntaxCheck | InvalidArg

let help_msg = "NAME: \n\tsmpl - Compiler for generating C code for SiMPLe" ^
        " Parallel Language\nARGUMENTS:\n\t -a code-input Print the parse" ^
        " results of the input SMPL code \n\t -c code-input Generate the C" ^
        " code equivalent to the input SMPL code\n\t -s code-input Just check" ^
        " the syntax of the SMPL code\n code-input can be one of the" ^
        " following:\n\t -t Take input from terminal\n\t <file-name> Take input" ^
        " from file\nEXAMPLES:\n\t./smplc -a -t : Take code input from" ^
        " terminal and print the parse results.\n\t./smplc -c hello.smpl >" ^
        " hello.c: Compile the SMPL code in hello.smpl and write the generated C" ^ 
        " code to hello.c\n\nAUTHORS:\n\tWritten" ^
        " by Ajay Siva Santosh, Andrei Papancea, Devashi Tandon\n\n" 

let process_program lexbuf action = 
    let expr = Parser.program Scanner.token lexbuf in
    let verified_env = Semantic_checker.validate_program expr in
    if (verified_env.Semantic_checker.valid_syntax) then
        match action with
        DumpAst -> 
            let result = Printast.parse_program expr in
            print_string result
        | CodeGen -> 
            let result = Compile.generate_code (expr) verified_env in
            print_string result
        | SyntaxCheck -> print_string "Validation is complete\n"
        | InvalidArg  -> print_string help_msg

let _ =
    if Array.length Sys.argv <> 3 then (
        print_string help_msg
    ) else (
        let action = try List.assoc Sys.argv.(1) 
            [ ("-a", DumpAst); ("-c", CodeGen); ("-s", SyntaxCheck)  ] 
        with Not_found -> InvalidArg in
        match action with
        InvalidArg -> print_string help_msg
        | _ ->
            let src_file = Sys.argv.(2) in  
            if(src_file = "-t") then
                let lexbuf = Lexing.from_channel stdin in
                process_program lexbuf action
            else
                let lexbuf = Lexing.from_channel (open_in src_file) in 
                process_program lexbuf action
    )
