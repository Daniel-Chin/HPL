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

let var_id_acc = ref 0;;

let getNewVarId () = let new_id = !var_id_acc in (
  var_id_acc := !var_id_acc + 1;
  new_id
);;

let emptyNamespace _ = None;;

let declare key namespace = let new_id = getNewVarId () in (
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

(* Semantic Domain *)
type boolean = | True | False | BoolError;;
type location = | ObjectId of int;;  (* -1 means null *);;
type value = 
| FieldValue of string
| IntValue of int
| LocationValue of location
| Closure of {var_id : int; cmd : ast; stack : stack}
and frame = | DeclFrame of (int * int * string) | CallFrame of ((int * int * string) * stack)
and stack = | Stack of frame list
and taintedValue = | ValError | TaintMissed of value
;;
module AnObject = Map.Make(String);;
type heapRow = 
| JustVal of taintedValue
| Everything of (taintedValue AnObject.t)
;;
type configuration = 
| Config of (ast * stack * (heapRow list))
| Halted of (stack * (heapRow list))
| ConfigError of (string * (ast * stack * (heapRow list)))
;;

let theNull = TaintMissed(LocationValue(ObjectId(-1)));;

exception OutOfHeapDom;;

let heapGet heap obj_id field_idt = 
  match List.nth heap obj_id with
  | JustVal(tva_) -> (
    if field_idt = "val" then tva_ else ValError
  )
  | Everything(map) -> (
    match AnObject.find_opt field_idt map with
    | Some x -> x
    | None -> theNull
  )
;;

let heapSet heap obj_id field_idt tva = 
let rec helper obj_id_acc field_idt tva = (
  function
  | [] -> []
  | h :: t -> (
    (if obj_id_acc = 0 then (
      match h with
      | JustVal(_) -> (
        if field_idt = "val" then JustVal(tva) 
        else raise OutOfHeapDom
      )
      | Everything(map) -> (
        Everything(AnObject.add field_idt tva map)
      )
    ) else h) :: 
    (helper (obj_id_acc - 1) field_idt tva t)
    )
) in helper obj_id field_idt tva heap
;;

let heapGrow heap is_malloc = (
  (
    if is_malloc then Everything(AnObject.empty) 
    else JustVal(theNull)
  ) :: heap
);;

let rec stackGet var_id = function
| Stack(lst_stack) -> (
  match lst_stack with
  | [] -> -99   (* Impossible *)
  | h :: t -> let helper (frame_var_id, frame_obj_id, _) = (
    if frame_var_id = var_id then frame_obj_id 
    else stackGet var_id (Stack(t))
  ) in (
    match h with
    | DeclFrame(binding) -> helper binding
    | CallFrame(binding, _) -> helper binding
  )
)
;;

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
  heapGet heap (stackGet id stack) "val"
)
| FieldSeek(a, b) -> (
  let tva_l = eval stack heap a
  and tva_f = eval stack heap b in
    match tva_l with
    | ValError -> ValError
    | TaintMissed(value) -> (
      match value with
      | LocationValue(location) -> (
        match location with
        | ObjectId(obj_id) -> (
          if obj_id != -1 then (
            match tva_f with
              | ValError -> ValError
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
  TaintMissed(Closure({var_id = id; cmd = cmd; stack = stack}))
)
| _ -> ValError   (* Impossible *)
;;

let evalBool stack heap expr = 
let helper = function 
| LiteralBool(x) -> Some x
| IsEqual(a, b) -> (
  let tva_a = eval stack heap a in
  let tva_b = eval stack heap b in
  match tva_a with
  | ValError -> None
  | TaintMissed(value_a) -> (
    match tva_b with
    | ValError -> None
    | TaintMissed(value_b) -> (
      match value_a with
      | IntValue(int_a) -> (
        match value_b with 
        | IntValue(int_b) -> Some (int_a = int_b)
        | _ -> None
      )
      | LocationValue(loc_a) -> (
        match value_b with 
        | LocationValue(loc_b) -> Some (loc_a = loc_b)
        | _ -> None
      )
      | Closure(clo_a) -> (
        match value_b with 
        | Closure(clo_b) -> Some (value_a = value_b)
        | _ -> None
      )
      | _ -> None
    )
  )
)
| IsLessThan(a, b) -> (
  let tva_a = eval stack heap a in
  let tva_b = eval stack heap b in
  match tva_a with
  | ValError -> None
  | TaintMissed(value_a) -> (
    match tva_b with
    | ValError -> None
    | TaintMissed(value_b) -> (
      match value_a with
      | IntValue(int_a) -> (
        match value_b with 
        | IntValue(int_b) -> Some (int_a < int_b)
        | _ -> None
      )
      | _ -> None
    )
  )
)
| _ -> failwith "Error 5028743087"  (* Impossible, thanks to parser*)
in match helper expr with
| Some true -> True
| Some false -> False
| None -> BoolError
;;

let obj_id_acc = ref 0;;

let getNewObjId () = let new_id = !obj_id_acc in (
  obj_id_acc := !obj_id_acc + 1;
  new_id
);;

let rec crank = function
| ConfigError(msg, _) -> failwith msg
| Halted(_) -> failwith "Cannot crank a halted config."
| Config(ctrl, stack, heap) -> (
  match ctrl with 
  | DecVar(VarAnnotation(var_idt, var_id), cmd) -> (
    let obj_id = getNewObjId () in
    match stack with
    | Stack(lst_stack) -> Config(
      Block(cmd), 
      Stack(DeclFrame(var_id, obj_id, var_idt) :: lst_stack), 
      heapGrow heap false
    )
  )
  | Block(cmd) -> (
    match crank (Config(cmd, stack, heap)) with
    | ConfigError(x) -> ConfigError(x)
    | Config(cmd_p, stack_p, heap_p) -> Config(
      Block(cmd_p), stack_p, heap_p
    )
    | Halted(stack_p, heap_p) -> Halted((
      match stack_p with
      | Stack(lst_stack) -> (
        match lst_stack with
        | [] -> failwith "Impossible to reach 26004978165"
        | DeclFrame(_) :: t -> Stack(t)
        | CallFrame(_, stashedStack) :: _ -> stashedStack
      )
    ), heap_p)
  )
  | ProcCall(proc, arg) -> (
    let tva_proc = eval stack heap proc
    and tva_arg  = eval stack heap arg in (
      match tva_proc with
      | ValError -> ConfigError("Calling `error` as proc", (ctrl, stack, heap))
      | TaintMissed(value) -> (
        match value with
        | Closure({var_id = var_id; cmd = cmd; stack = defStack}) -> (
          let obj_id = getNewObjId () in
          match defStack with
          | Stack(lst_stack) -> Config(
            Block(cmd), 
            Stack(CallFrame((var_id, obj_id, "__FuncArg__"), stack) :: lst_stack), 
            heapSet (heapGrow heap false) obj_id "val" tva_arg
          )
        )
        | _ -> ConfigError("Calling a non-proc", (ctrl, stack, heap))
      )
    )
  )
  | VarAssign(VarAnnotation(_, var_id), expr) -> (
    match eval stack heap expr with
    | ValError -> ConfigError("Cannot assign `error` to variable", (ctrl, stack, heap))
    | TaintMissed(value) -> (
      let obj_id = stackGet var_id stack in Halted(
        stack, 
        heapSet heap obj_id "val" (TaintMissed(value))
      )
    )
  )
  | FieldAssign(obj, field, expr) -> (
    let tva_l = eval stack heap obj
    and tva_f = eval stack heap field
    and tva_e = eval stack heap expr in 
      match tva_l with
      | ValError -> ConfigError("During field assignment, the l.h.s. of the dot is `error`", (ctrl, stack, heap))
      | TaintMissed(value_l) -> (
        match tva_f with
        | ValError -> ConfigError("During field assignment, the r.h.s. of the dot is `error`", (ctrl, stack, heap))
        | TaintMissed(value_f) -> (
          match value_l with
          | LocationValue(ObjectId(obj_id)) -> (
            match value_f with
            | FieldValue(field_idt) -> (
              try
                Halted(
                  stack, 
                  heapSet heap obj_id field_idt tva_e
                )
              with OutOfHeapDom -> ConfigError("Field assignment out of heap domain", (ctrl, stack, heap))
            )
            | _ -> ConfigError("During field assignment, the r.h.s. of the dot is non-field", (ctrl, stack, heap))
          )
          | _ -> ConfigError("During field assignment, the l.h.s. of the dot is non-location", (ctrl, stack, heap))
        )
      )
  )
  | Malloc(VarAnnotation(_, var_id)) -> (
    let obj_id = getNewObjId () in
    let heap_p = heapSet heap (
      stackGet var_id stack
    ) "val" (TaintMissed(LocationValue(ObjectId(obj_id)))) in
    Halted(
      stack, 
      heapGrow heap_p true
    )
  )
  | Skip -> Halted(stack, heap)
  | FirstThen(cmd1, cmd2) -> (
    match crank (Config(cmd1, stack, heap)) with
    | ConfigError(x) -> ConfigError(x)
    | Config(cmd1_p, stack_p, heap_p) -> Config(
      FirstThen(cmd1_p, cmd2), stack_p, heap_p
    )
    | Halted(stack_p, heap_p) -> Config(cmd2, stack_p, heap_p)
  )
  | WhileLoop(condition, cmd) -> (
    match evalBool stack heap condition with
    | BoolError -> ConfigError("while loop condition is `error`", (ctrl, stack, heap))
    | True -> (
      Config((FirstThen(cmd, WhileLoop(condition, cmd))), stack, heap)
    )
    | False -> Halted(stack, heap)
  )
  | IfThenElse(condition, thenClause, elseClause) -> (
    match evalBool stack heap condition with
    | BoolError -> ConfigError("if statement condition is `error`", (ctrl, stack, heap))
    | True -> Config(thenClause, stack, heap)
    | False -> Config(elseClause, stack, heap)
  )
  | Parallel(cmd1, cmd2) -> (
    let helper cmd_a cmd_b = (
      match crank (Config(cmd_a, stack, heap)) with
      | ConfigError(x) -> ConfigError(x)
      | Config(cmd_a_p, stack_p, heap_p) -> Config(
        Parallel(cmd_a_p, cmd_b), stack_p, heap_p
      )
      | Halted(stack_p, heap_p) -> Config(cmd_b, stack_p, heap_p)
    ) in if Random.bool () 
    then helper cmd1 cmd2
    else helper cmd2 cmd1
  )
  | Atomic(cmd) -> (
    let rec helper cmd_x stack_x heap_x = (
      match crank (Config(cmd_x, stack_x, heap_x)) with
      | ConfigError(x) -> ConfigError(x)
      | Config(cmd_p, stack_p, heap_p) -> helper cmd_p stack_p heap_p
      | Halted(stack_p, heap_p) -> Halted(stack_p, heap_p)
    ) in helper cmd stack heap
  )
  | _ -> failwith "Error 2375098742307"   (* Impossible, thanks to parser *)
);;

let printTva = function
| ValError -> print_string "`error` \n"
| TaintMissed(value) -> (
  match value with
  | FieldValue(field_idt) -> (
    print_string "field '";
    print_string field_idt;
    print_string "' \n"
  )
  | IntValue(x) -> (
    print_string "int ";
    print_int x;
    print_newline
  )
  | LocationValue(target_obj_id) -> (
    print_string "<obj @ ";
    print_int target_obj_id;
    print_string "> \n"
  )
  | Closure(_) -> (
    print_string "some closure \n";
  )
)
;;

let rec printStack = let helper (var_id, obj_id, var_idt) = (
  print_string var_idt; 
  print_string "_";
  print_int var_id; 
  print_string "\t <obj @ ";
  print_int obj_id;
  print_string "> \n"
) in function
| [] -> ()
| h :: t -> (
  match h with
  | DeclFrame(binding)    -> helper binding
  | CallFrame(binding, _) -> helper binding
  ;
  printStack t
)
;;

let rec printHeap obj_id = function
| [] -> ()
| h :: t -> (
  print_string "\t <obj @ ";
  print_int obj_id;
  print_string "> \n";
  match h with
  | JustVal(tVal) -> (
    print_string "  val : ";
    printTva tVal
  )
  | Everything(map) -> (
    AnObject.iter (function key value -> (
      print_string "  ";
      print_string key;
      print_string " : ";
      printTva value
    )) map
  )
  ;
  printHeap (obj_id + 1) t
)
;;

let rec pprintObj heap depth already obj_id = (
  match List.nth heap obj_id with
  | JustVal(tva_) -> printTva tva_
  | Everything(map) -> (
    print_string "{ \n";
    AnObject.iter (function k v -> (
      print_string String.make (depth + 1) ' ';
      print_string k;
      print_string " : ";
      match v with
      | ValError -> print_string "`error` \n"
      | TaintMissed(value) -> (
        match value with
        | FieldValue(_) | IntValue(_) | Closure(_) -> printTva v
        | LocationValue(target_obj_id) -> (
          let _already = obj_id :: already in 
          if List.mem target_obj_id _already 
          then (
            print_string "recursive <obj @ ";
            print_int target_obj_id;
            print_string "> \n";
          )
          else pprintObj heap (depth + 1) _already target_obj_id
        )
      )
    )) map;
    print_string String.make depth ' ';
    print_string "} @ ";
    print_int obj_id;
    print_newline
  )
);;

let rec pprintVars heap = let helper (var_id, obj_id, var_idt) = (
  print_string var_idt; 
  print_string "_";
  print_int var_id; 
  print_string " = ";
  pprintObj heap 0 [] obj_id
) in function
| [] -> ()
| h :: t -> (
  match h with
  | DeclFrame(binding)    -> helper binding
  | CallFrame(binding, _) -> helper binding
  ;
  printStack t
)
;;

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
