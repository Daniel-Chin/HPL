/* File MENHIR.mly */

%{ (* header *)

%} /* declarations */

/* lexer tokens */
%token SEMICOLON ASSIGN MINUS DOT COLUMN EQUAL LESSTHAN
%token LPAREN RPAREN LBRAK RBRAK DECLARE MALLOC SKIP
%token WHILE IF ELSE PARALLEL ATOM PROC NULL THE_END
%token < string > VARIDT FIELDIDT
%token < int > NUM
%token < bool > BOOL
%start prog                   /* the entry point */
%type <Types.ast> prog  
%type <Types.ast> cmd
%type <Types.ast> expr
%type <Types.ast> bool
%left ASSIGN
%left MINUS
%left DOT

%% /* rules */

prog :
    c = cmd THE_END  { c }

cmd :
    DECLARE v = VARIDT SEMICOLON c = cmd     { DecVar((Types.VarAnnotation(v, -1), c)) }
  | p = expr LPAREN a = expr RPAREN          { ProcCall((p, a)) }
  | MALLOC LPAREN v = VARIDT RPAREN          { Malloc(Types.VarAnnotation(v, -1)) }
  | v = VARIDT ASSIGN e = expr               { VarAssign((Types.VarAnnotation(v, -1), e)) }
  | o = expr DOT f = expr ASSIGN e = expr    { FieldAssign((o, f, e)) }
  | SKIP                                     { Skip }
  | LBRAK c1 = cmd SEMICOLON c2 = cmd RBRAK  { FirstThen((c1, c2)) }
  | WHILE b = bool c = cmd                   { WhileLoop((b, c)) }
  | IF b = bool t = cmd ELSE e = cmd         { IfThenElse((b, t, e)) }
  | LBRAK c1 = cmd PARALLEL c2 = cmd RBRAK   { Parallel((c1, c2)) }
  | ATOM LPAREN c = cmd RPAREN               { Atomic(c) }

expr :
    f = FIELDIDT                    { FieldIdt(f) }
  | n = NUM                         { LiteralNum(n) }
  | e1 = expr MINUS e2 = expr       { Minus((e1, e2)) }
  | NULL                            { Null }
  | v = VARIDT                      { VarIdt(Types.VarAnnotation(v, -1)) }
  | e1 = expr DOT e2 = expr         { FieldSeek((e1, e2)) }
  | PROC v = VARIDT COLUMN c = cmd  { ProcDef((Types.VarAnnotation(v, -1), c)) }

bool :
    b = BOOL                      { LiteralBool(b) }
  | e1 = expr EQUAL e2 = expr     { IsEqual((e1, e2)) }
  | e1 = expr LESSTHAN e2 = expr  { IsLessThan((e1, e2)) }

%% (* trailer *)
