open Sprout_ast
open Format

let print_program fmt prog = (Printf.printf " \nwhat is in side => \n" ;
	Format.set_max_indent 100;
	printTypedefList fmt prog.typedefs;
	printFuncdefList fmt prog.funcdefs
)
