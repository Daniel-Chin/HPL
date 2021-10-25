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
      print_string "and then \n"; 
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
    | FieldIdt(field_idt) -> (
      indent ();
      print_string "field identifier ["; 
      print_string field_idt; 
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
    | Block(cmd) -> (
      indent ();
      print_string "BLOCK \n";
      prettyPrint (depth + 1) cmd
    )
;;

exception UsingUndeclaredVariable of string;;

let id_acc = ref 0;;

let getNewId () = let new_id = !id_acc in (
  id_acc := !id_acc + 1;
  new_id
);;

let emptyNamespace _ = None;;

let declare key namespace = let new_id = getNewId () in (
  (fun key_p -> (
    if key_p = key then Some new_id else namespace key_p
  )), 
  new_id
);;

let rec annotate namespace = function 
    | DecVar(VarAnnotation(name, _), cmd) -> (
      let new_namespace, new_id = declare name namespace in
      DecVar(
        VarAnnotation(name, new_id), 
        annotate new_namespace cmd
      )
    )
    | ProcCall(proc, arg) -> ProcCall(
      annotate namespace proc, 
      annotate namespace arg
    )
    | Malloc(VarAnnotation(name, _)) -> (
      match namespace name with
        | None -> raise (UsingUndeclaredVariable name)
        | Some id -> Malloc(VarAnnotation(name, id))
    )
    | VarAssign(VarAnnotation(name, _), expr) -> (
      match namespace name with
        | None -> raise (UsingUndeclaredVariable name)
        | Some id -> VarAssign(
          VarAnnotation(name, id), 
          annotate namespace expr
        )
    )
    | FirstThen(cmd1, cmd2) -> FirstThen(
      annotate namespace cmd1, 
      annotate namespace cmd2
    )
    | FieldAssign(obj, field, expr) -> FieldAssign(
      annotate namespace obj, 
      annotate namespace field, 
      annotate namespace expr
    )
    | Skip -> Skip
    | WhileLoop(condition, cmd) -> WhileLoop(
      annotate namespace condition, 
      annotate namespace cmd 
    )
    | IfThenElse(condition, thenClause, elseClause) -> IfThenElse(
      annotate namespace condition, 
      annotate namespace thenClause, 
      annotate namespace elseClause
    )
    | Parallel(cmd1, cmd2) -> Parallel(
      annotate namespace cmd1, 
      annotate namespace cmd2
    )
    | Atomic(cmd) -> Atomic(
      annotate namespace cmd
    )
    | FieldIdt(field_idt) -> FieldIdt(field_idt)
    | LiteralNum(x) -> LiteralNum(x)
    | Minus(expr1, expr2) -> Minus(
      annotate namespace expr1, 
      annotate namespace expr2
    )
    | Null -> Null
    | VarIdt(VarAnnotation(name, _)) -> (
      match namespace name with
        | None -> raise (UsingUndeclaredVariable name)
        | Some id -> VarIdt(VarAnnotation(name, id))
    )
    | FieldSeek(obj, field) -> FieldSeek(
      annotate namespace obj, 
      annotate namespace field
    )
    | ProcDef(VarAnnotation(name, _), cmd) -> (
      let new_namespace, new_id = declare name namespace in
      ProcDef(
        VarAnnotation(name, new_id), 
        annotate new_namespace cmd
      )
    )
    | LiteralBool(x) -> LiteralBool(x)
    | IsEqual(expr1, expr2) -> IsEqual(
      annotate namespace expr1, 
      annotate namespace expr2
    )
    | IsLessThan(expr1, expr2) -> IsLessThan(
      annotate namespace expr1, 
      annotate namespace expr2
    )
    | Block(_) -> failwith "Cannot annotate Block"
;;

type boolean = | True | False | BoolError;;
type location = | ObjectId of int;;  (* -1 means null *);;
type value = 
| FieldValue of string
| IntValue of int
| LocationValue of location
| Closure of { var : int; cmd : ast; stack : stack }
and frame = | DeclFrame of (int * int) | CallFrame of ((int * int) * stack)
and stack = | Stack of frame list
and taintedValue = | ValError | TaintMissed of value
type configuration = 
| Config of (ast * stack * frame)
| Halted of (stack * frame)
| ConfigError
;;

module AnObject = Map.Make(String);;

let theNull = TaintMissed(LocationValue(ObjectId(-1)));;

let heapGet heap obj_id field_idt = 
  match AnObject.find_opt field_idt (List.nth heap obj_id) with
  | Some x -> x
  | None -> ValError
;;

let rec stackGet var_id = function
| Stack(stk) -> (
  match stk with
  | [] -> -99   (* Impossible *)
  | h :: t -> let helper (frame_var_id, frame_obj_id) = (
    if frame_var_id = var_id then frame_obj_id 
    else stackGet var_id (Stack(t))
  ) in (
    match h with
    | DeclFrame(binding) -> helper binding
    | CallFrame(binding, _) -> helper binding
  )
)

let rec eval stack heap = function
| FieldIdt(field_idt) -> TaintMissed(FieldValue(field_idt))
| LiteralNum(x) -> TaintMissed(IntValue(x))
| Minus(a, b) -> (
  match eval stack heap a with 
  | ValError -> ValError
  | TaintMissed(value) -> match value with
    | IntValue(int_a) -> (
      match eval stack heap b with 
      | ValError -> ValError
      | TaintMissed(value) -> match value with
        | IntValue(int_b) -> TaintMissed(IntValue(int_a - int_b))
        | _ -> ValError
    )
    | _ -> ValError
)
| Null -> theNull
| VarIdt(VarAnnotation(_, id)) -> (
  heapGet heap (stackGet id stack) ""
)
| FieldSeek(a, b) -> (
  let tva_l = eval stack heap a
  and tva_f = eval stack heap b in
    match tva_l with
    | ValError -> ValError  (* impossible *)
    | TaintMissed(value) -> (
      match value with
      | LocationValue(location) -> (
        match location with
        | ObjectId(obj_id) -> (
          if obj_id != -1 then (
            match tva_f with
              | ValError -> ValError  (* impossible *)
              | TaintMissed(value) -> match value with
                | FieldValue(field_idt) -> (
                  heapGet heap obj_id field_idt
                )
                | _ -> ValError
          ) else ValError
        )
      )
      | _ -> ValError
    )
)
| ProcDef(VarAnnotation(_, id), cmd) -> (
  TaintMissed(Closure({var = id; cmd = cmd; stack = stack}))
)
| _ -> ValError   (* Impossible *)
;;

let rec crank = function
| ConfigError -> failwith "ERROR 327169437652"
| Halted -> failwith "Cannot crank a halted config."
| configuration(ctrl, stack, heap) -> (
  match ctrl with 
  | DecVar(var_annotation, cmd) -> (
  )
  | ProcCall(proc, arg) -> (
  )
  | Malloc(var_annotation) -> (
  )
  | VarAssign(var_annotation, expr) -> (
  )
  | FirstThen(cmd1, cmd2) -> (
  )
  | FieldAssign(obj, field, expr) -> (
  )
  | Skip -> Skip
  | WhileLoop(condition, cmd) -> (
  )
  | IfThenElse(condition, thenClause, elseClause) -> (
  )
  | Parallel(cmd1, cmd2) -> (
  )
  | Atomic(cmd) -> (
  )
  | FieldIdt(field_idt) -> ()
  | LiteralNum(x) -> ()
  | Minus(expr1, expr2) -> (
  )
  | Null -> Null
  | VarIdt(var_annotation) -> (
  )
  | FieldSeek(obj, field) -> (
  )
  | ProcDef(var_annotation, cmd) -> (
  )
  | LiteralBool(x) -> ()
  | IsEqual(expr1, expr2) -> (
  )
  | IsLessThan(expr1, expr2) -> (
  )
  | Block(_) -> ()
)

let lexbuf = Lexing.from_channel stdin in
try
  while true do
    try
      let theAst = MENHIR.prog LEX.token lexbuf in (
        prettyPrint 0 (annotate emptyNamespace theAst)
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
