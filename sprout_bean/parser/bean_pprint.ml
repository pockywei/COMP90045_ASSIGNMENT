(** The is the file contains our pretty print method to nicely print out the 
 * bean language.
 * Program Description : This program is for the project of COMP90045 
 * at the University of Melbourne,
 *it is a compiler program for the bean language
 *
 * Team Member : 
 * Angus Huang 640386
 * Bingfeng Liu 639187
 * Chesdametrey Seng 748852
 * Chenhao Wei 803931
 *
 * Project Created Date : 18.03.2016
 *)


open Bean_ast
open Format

let print_program fmt prog = (
    (*set max indent, other wise the indentation might be limited*)
    Format.set_max_indent 100; 
    (*start print typedef part*)
    printTypedefList fmt prog.typedefs;
    (*start print method declaration part*)
    printFuncdefList fmt prog.funcdefs
)
