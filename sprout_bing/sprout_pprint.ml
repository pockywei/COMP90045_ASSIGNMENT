open Sprout_ast
open Format

let print_program fmt prog = 
(Printf.printf " \n what is in side => " 
;print_decls_list prog.decls;Printf.printf "\n" ; print_stmts prog.stmts)
