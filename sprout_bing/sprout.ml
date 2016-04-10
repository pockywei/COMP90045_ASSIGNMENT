open Sprout_lex
module P = Sprout_parse

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

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  try
      (* Open the input file *)
      let infile = match !infile_name with
      | None -> stdin
      | Some fname -> begin Printf.printf "fname => %s \n" fname;open_in fname end in
      (* Initialize lexing buffer *)
      let lexbuf = Lexing.from_channel infile in
      (* Call the parser *)
      let prog = Sprout_parse.start_state Sprout_lex.token lexbuf in
      match !mode with
      | PrettyPrint ->
        Sprout_pprint.print_program Format.std_formatter prog 
      | Compile -> Printf.eprintf "Sorry, cannot compile yet."
  with
      | Parsing.Parse_error -> Printf.eprintf "Syntax error\n"
      | LexFail lexbuf -> let position = Lexing.lexeme_start_p lexbuf in
          Printf.eprintf "Illegal Character on line %d, col %d.\n" position.Lexing.pos_lnum
          (position.Lexing.pos_cnum - position.Lexing.pos_bol)

let _ = main ()
