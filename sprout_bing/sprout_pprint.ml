open Sprout_ast
open Format

let print_program fmt prog = (Printf.printf " \nwhat is in side => \n" ;
	printTypedefList fmt prog.typedefs;
	printFuncdefList fmt prog.funcdefs
)
