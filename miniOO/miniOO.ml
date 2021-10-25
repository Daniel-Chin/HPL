(* File miniOO.ml *)
open Parsing;;
open Types;;

let rec prettyPrint depth = 
  let indent () = print_string (
    String.make depth ' '
  ) and printVarAnnotation = function
    | VarAnnotation(name, id) -> (
      print_string "<";
      print_string name;
      print_string " (";
      print_int id;
      print_string ")>"
    )
  in function
    | DecVar(var_annotation, cmd) -> (
      indent ();
      print_string "declare var "; 
      printVarAnnotation var_annotation; 
      print_string " in \n"; 
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
    | Malloc(var_annotation) -> (
      indent ();
      print_string "malloc "; 
      printVarAnnotation var_annotation; 
      print_string " \n"
    )
    | VarAssign(var_annotation, expr) -> (
      indent ();
      print_string "assign to var "; 
      printVarAnnotation var_annotation; 
      print_string " = \n"; 
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
    | VarIdt(var_annotation) -> (
      indent ();
      print_string "var identifier "; 
      printVarAnnotation var_annotation; 
      print_string " \n"
    )
    | FieldSeek(obj, field) -> (
      indent ();
      print_string "seeking in object \n"; 
      prettyPrint (depth + 1) obj;
      indent ();
      print_string "the field \n"; 
      prettyPrint (depth + 1) field
    )
    | ProcDef(var_annotation, cmd) -> (
      indent ();
      print_string "proc taking "; 
      printVarAnnotation var_annotation; 
      print_string " that does \n"; 
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

let emptyNamespace _ = None;;
let declare key namespace next_id = fun key_p -> (
  if key_p = key then Some next_id else namespace key_p
);;

exception UsingUndeclaredVariable of string;;

let rec annotate namespace next_id = function 
    | DecVar(var_annotation, cmd) -> (match var_annotation with
      | VarAnnotation(name, _) -> (
        let new_namespace = declare name namespace next_id in
        DecVar(
          VarAnnotation(name, next_id), 
          annotate new_namespace (next_id + 1) cmd
        )
      )
    )
    | ProcCall(proc, arg) -> ProcCall(
      annotate namespace next_id proc, 
      annotate namespace next_id arg
    )
    | Malloc(var_annotation) -> (match var_annotation with
      | VarAnnotation(name, _) -> (
        match namespace name with
          | None -> raise (UsingUndeclaredVariable name)
          | Some id -> Malloc(VarAnnotation(name, id))
      )
    )
    | VarAssign(var_annotation, expr) -> (match var_annotation with
      | VarAnnotation(name, _) -> (
        match namespace name with
          | None -> raise (UsingUndeclaredVariable name)
          | Some id -> VarAssign(
            VarAnnotation(name, id), 
            annotate namespace next_id expr
          )
      )
    )
    | FirstThen(cmd1, cmd2) -> FirstThen(
      annotate namespace next_id cmd1, 
      annotate namespace next_id cmd2
    )
    | FieldAssign(obj, field, expr) -> FieldAssign(
      annotate namespace next_id obj, 
      annotate namespace next_id field, 
      annotate namespace next_id expr
    )
    | Skip -> Skip
    | WhileLoop(condition, cmd) -> WhileLoop(
      annotate namespace next_id condition, 
      annotate namespace next_id cmd 
    )
    | IfThenElse(condition, thenClause, elseClause) -> IfThenElse(
      annotate namespace next_id condition, 
      annotate namespace next_id thenClause, 
      annotate namespace next_id elseClause
    )
    | Parallel(cmd1, cmd2) -> Parallel(
      annotate namespace next_id cmd1, 
      annotate namespace next_id cmd2
    )
    | Atomic(cmd) -> Atomic(
      annotate namespace next_id cmd
    )
    | FieldIdt(filed_idt) -> FieldIdt(filed_idt)
    | LiteralNum(x) -> LiteralNum(x)
    | Minus(expr1, expr2) -> Minus(
      annotate namespace next_id expr1, 
      annotate namespace next_id expr2
    )
    | Null -> Null
    | VarIdt(var_annotation) -> (match var_annotation with
      | VarAnnotation(name, _) -> (
        match namespace name with
          | None -> raise (UsingUndeclaredVariable name)
          | Some id -> VarIdt(VarAnnotation(name, id))
      )
    )
    | FieldSeek(obj, field) -> FieldSeek(
      annotate namespace next_id obj, 
      annotate namespace next_id field
    )
    | ProcDef(var_annotation, cmd) -> (match var_annotation with
      | VarAnnotation(name, _) -> (
        let new_namespace = declare name namespace next_id in
        ProcDef(
          VarAnnotation(name, next_id), 
          annotate new_namespace (next_id + 1) cmd
        )
      )
    )
    | LiteralBool(x) -> LiteralBool(x)
    | IsEqual(expr1, expr2) -> IsEqual(
      annotate namespace next_id expr1, 
      annotate namespace next_id expr2
    )
    | IsLessThan(expr1, expr2) -> IsLessThan(
      annotate namespace next_id expr1, 
      annotate namespace next_id expr2
    )
;;

let lexbuf = Lexing.from_channel stdin in
try
  while true do
    try
      let theAst = MENHIR.prog LEX.token lexbuf in (
        prettyPrint 0 (annotate (emptyNamespace) 0 theAst)
      )
    with Parse_error ->
      (
        (print_string "Syntax error ..." ; print_newline ()) ;
        clear_parser ()
      )
  done
with 
  | LEX.Eof -> ()
  | UsingUndeclaredVariable(msg) -> raise (UsingUndeclaredVariable msg)
  | e -> (
    print_string "Unknown error. Prabably parsing error? \n";
    print_string "lexbuf is at char # ";
    print_int lexbuf.lex_curr_pos;
    print_string "\n (Use ctrl+alt+G in VSCode to seek char pos.)";
    raise e
  )
;;
