/* File MENHIR.mly */

%{ (* header *)
  type ast =
    | Assign    of (ast    * ast)
    | ProcCall  of (ast    * ast)
    | FirstThen of (ast    * ast)
    | Parallel  of (ast    * ast)
    | DecVar    of (string * ast)
    | Malloc    of (string)
    | Skip
    | WhileLoop of (unit)
  ;;
%} /* declarations */

/* lexer tokens */
%token EOL SEMICOLON ASSIGN MINUS DOT COLUMN EQUAL LESSTHAN
%token LPAREN RPAREN LBRAK RBRAK DECLARE MALLOC SKIP
%token WHILE IF ELSE PARALLEL ATOM PROC NULL
%token < string > VARIDT FIELDIDT
%token < int > NUM
%token < bool > BOOL
%start prog                   /* the entry point */
%type <unit> prog  
%type <unit> cmd
%type <unit> expr
%type <unit> bool
%left ASSIGN
%left MINUS
%left DOT

%% /* rules */

prog :
    cmd EOL  { () }

cmd :
    DECLARE VARIDT SEMICOLON cmd  { () }
  | expr LPAREN expr RPAREN       { () }
  | MALLOC LPAREN VARIDT RPAREN   { () }
  | VARIDT ASSIGN expr            { () }
  | expr DOT expr ASSIGN expr     { () }
  | SKIP                          { () }
  | LBRAK cmd SEMICOLON cmd RBRAK { () }
  | WHILE bool cmd                { () }
  | IF bool cmd ELSE cmd          { () }
  | LBRAK cmd PARALLEL cmd RBRAK  { () }
  | ATOM LPAREN cmd RPAREN        { () }

expr :
    FIELDIDT                          { () }
  | NUM                               { () }
  | expr MINUS expr                   { () }
  | NULL                              { () }
  | VARIDT                            { () }
  | expr DOT expr                     { () }
  | PROC VARIDT COLUMN cmd            { () }

bool :
    BOOL                              { () }
  | expr EQUAL expr                   { () }
  | expr LESSTHAN expr                { () }

%% (* trailer *)
