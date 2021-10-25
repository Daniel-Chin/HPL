(* File miniOO.ml *)
open Parsing;;
open Types;;

let rec prettyPrint depth = 
  let indent () = print_string(String.make depth ' ') in function
    | DecVar(x, body) -> (
      indent();
      print_string("declare var "); 
      print_string(x); 
      print_string(" in \n"); 
      prettyPrint (depth + 1) body
    )
    | ProcCall(proc, arg) -> (
      indent();
      print_string("calling proc \n"); 
      prettyPrint (depth + 1) proc;
      indent();
      print_string("with argument \n"); 
      prettyPrint (depth + 1) arg
    )
    | Malloc(var_name) -> (
      indent();
      print_string("malloc "); 
      print_string(var_name); 
      print_newline()
    )
    | VarAssign(var_name, expr) -> (
      indent();
      print_string("assign to "); 
      print_string(var_name); 
      print_string(" with \n"); 
      prettyPrint (depth + 1) expr
    )
    | FirstThen(body1, body2) -> (
      indent();
      print_string("first \n"); 
      prettyPrint (depth + 1) body1;
      indent();
      print_string("then \n"); 
      prettyPrint (depth + 1) body2
    )
    (* | FieldAssign of (ast * ast * ast)
    | Skip
    | WhileLoop   of (ast * ast)
    | IfThenElse  of (ast * ast * ast)
    | Parallel    of (ast * ast)
    | Atomic      of (ast)

    | FieldIdt    of (string)
    | LiteralNum  of (int)
    | Minus       of (ast * ast)
    | Null
    | VarIdt      of (string)
    | FieldSeek   of (ast * ast)
    | ProcDef     of (string * ast)

    | LiteralBool of (bool)
    | IsEqual     of (ast * ast)
    | IsLessThan  of (ast * ast) *)
    | _ -> (
      indent();
      print_string("unknown \n")
    )
;;

try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      let theAst = MENHIR.prog LEX.token lexbuf in 
      prettyPrint 0 theAst
    with Parse_error ->
      (
        (print_string "Syntax error ..." ; print_newline ()) ;
        clear_parser ()
      )
  done
with LEX.Eof ->
  ()
;;
