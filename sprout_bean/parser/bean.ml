(** The is the file contains our main method to run the whole program
 * Program Description : This program is for the project of COMP90045 
 * at the University of Melbourne,
 * it is a compiler program for the bean language
 *
 * Team Member : 
 * Angus Huang 640386
 * Bingfeng Liu 639187
 * Chesdametrey Seng 748852
 * Chenhao Wei 803931
 *
 * Project Created Date : 18.03.2016
 * Note that this file is provided as the skeleton code in this subject
 *)

open Bean_codegen
open Bean_lex
open Lexing
open Bean_symbol
open Bean_analyze
module P = Bean_parse

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]
  
let printError msg lexbuf =
    let position = lexeme_start_p lexbuf in
    Printf.eprintf "%s on line %d, column %d.\n" msg position.pos_lnum (position.pos_cnum - position.pos_bol + 1)

let main () =
    (* Parse the command-line arguments *)
    Arg.parse speclist
        (begin fun fname -> infile_name := Some fname end)
        "bean [-p] [bean source]" ;
        (* Open the input file *)
        let infile = match !infile_name with
        | None -> stdin
        | Some fname -> open_in fname  in
        (* Initialize lexing buffer *)
        let lexbuf = Lexing.from_channel infile in
        try
            (* Call the parser *)
            let prog = Bean_parse.start_state Bean_lex.token lexbuf in
            match !mode with
            | PrettyPrint ->Printf.printf "pretty print disable\n"
            (*start_test_analyzer prog*)
                (*codegen_stmts (List.hd prog.funcdefs)*)
              (*Bean_pprint.print_program Format.std_formatter prog *)

            | Compile -> start_test_analyzer prog
        with
            (* Handle suitable message for parsing and lexing error *)
            | Parsing.Parse_error -> printError "Syntax error" lexbuf
            | LexFail -> printError "Invalid character" lexbuf

let _ = main ()
