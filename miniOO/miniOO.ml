(* File miniOO.ml *)
open Parsing;;
open Types;;

let rec prettyPrint depth = 
  let indent () = print_string (
    String.make depth ' '
  ) in function
    | DecVar(var_idt, cmd) -> (
      indent ();
      print_string "declare var <"; 
      print_string var_idt; 
      print_string "> in \n"; 
      prettyPrint (depth + 1) cmd
    )
    | ProcCall(proc, arg) -> (
      indent ();
      print_string "call proc \n"; 
      prettyPrint (depth + 1) proc;
      indent ();
      print_string "with argument \n"; 
      prettyPrint (depth + 1) arg
    )
    | Malloc(var_idt) -> (
      indent ();
      print_string "malloc <"; 
      print_string var_idt; 
      print_string "> \n"
    )
    | VarAssign(var_idt, expr) -> (
      indent ();
      print_string "assign to var <"; 
      print_string var_idt; 
      print_string "> = \n"; 
      prettyPrint (depth + 1) expr
    )
    | FirstThen(cmd1, cmd2) -> (
      indent ();
      print_string "first \n"; 
      prettyPrint (depth + 1) cmd1;
      indent ();
      print_string "then \n"; 
      prettyPrint (depth + 1) cmd2
    )
    | FieldAssign(obj, field, expr) -> (
      indent ();
      print_string "assign to field \n"; 
      prettyPrint (depth + 1) obj;
      indent ();
      print_string ". \n"; 
      prettyPrint (depth + 1) field;
      indent ();
      print_string "= \n"; 
      prettyPrint (depth + 1) expr
    )
    | Skip -> (
      indent ();
      print_string "skip \n"
    )
    | WhileLoop(condition, cmd) -> (
      indent ();
      print_string "while \n"; 
      prettyPrint (depth + 1) condition;
      indent ();
      print_string "do \n";
      prettyPrint (depth + 1) cmd
    )
    | IfThenElse(condition, thenClause, elseClause) -> (
      indent ();
      print_string "if \n"; 
      prettyPrint (depth + 1) condition;
      indent ();
      print_string "then \n"; 
      prettyPrint (depth + 1) thenClause;
      indent ();
      print_string "else \n"; 
      prettyPrint (depth + 1) elseClause
    )
    | Parallel(cmd1, cmd2) -> (
      indent ();
      print_string "parallel left \n"; 
      prettyPrint (depth + 1) cmd1;
      indent ();
      print_string "parallel right \n"; 
      prettyPrint (depth + 1) cmd2
    )
    | Atomic(cmd) -> (
      indent ();
      print_string "atomic \n"; 
      prettyPrint (depth + 1) cmd
    )
    | FieldIdt(filed_idt) -> (
      indent ();
      print_string "field identifier ["; 
      print_string filed_idt; 
      print_string "] \n"
    )
    | LiteralNum(x) -> (
      indent ();
      print_string "literally "; 
      print_int x; 
      print_newline ()
    )
    | Minus(expr1, expr2) -> (
      indent ();
      print_string "from \n";
      prettyPrint (depth + 1) expr1;
      indent ();
      print_string "subtract \n";
      prettyPrint (depth + 1) expr2
    )
    | Null -> (
      indent ();
      print_string "null \n"
    )
    | VarIdt(var_idt) -> (
      indent ();
      print_string "var identifier <"; 
      print_string var_idt; 
      print_string "> \n"
    )
    | FieldSeek(obj, field) -> (
      indent ();
      print_string "seeking in object \n"; 
      prettyPrint (depth + 1) obj;
      indent ();
      print_string "the field \n"; 
      prettyPrint (depth + 1) field
    )
    | ProcDef(arg_name, cmd) -> (
      indent ();
      print_string "proc taking <"; 
      print_string arg_name; 
      print_string "> that does \n"; 
      prettyPrint (depth + 1) cmd
    )
    | LiteralBool(x) -> (
      indent ();
      print_string "literally "; 
      print_string (if x then "true" else "false");
      print_newline ()
    )
    | IsEqual(expr1, expr2) -> (
      indent ();
      print_string "whether \n";
      prettyPrint (depth + 1) expr1;
      indent ();
      print_string "equals \n";
      prettyPrint (depth + 1) expr2
    )
    | IsLessThan(expr1, expr2) -> (
      indent ();
      print_string "whether \n";
      prettyPrint (depth + 1) expr1;
      indent ();
      print_string "is less than \n";
      prettyPrint (depth + 1) expr2
    )
;;

let lexbuf = Lexing.from_channel stdin in
try
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
with 
  | LEX.Eof -> ()
  | e -> (
    print_string "lexbuf is at ";
    print_int lexbuf.lex_curr_pos;
    print_string "\n";
    raise e
  )
;;
