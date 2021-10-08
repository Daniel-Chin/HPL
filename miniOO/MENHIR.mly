/* File MENHIR.mly */

%{ (* header *)
  
%} /* declarations */

/* lexer tokens */
%token EOL SEMICOLON ASSIGN MINUS DOT COLUMN EQUAL LESSTHAN
%token LPAREN RPAREN LBRAK RBRAK DECLARE MALLOC SKIP
%token WHILE IF ELSE PARALLEL ATOM PROC
%token < string > VARIDT FIELDIDT
%token < int > NUM
%token < bool > BOOL
%start prog                   /* the entry point */
%type <unit> prog  
%type <unit> cmd
%type <unit> assign
%type <unit> expr
%type <unit> slot
%left COLUMN
%left MINUS LESSTHAN EQUAL

%% /* rules */

prog :
    cmd EOL  { () }
	
cmd :
    assign                        { () }
  | expr LPAREN expr RPAREN       { () }
  | LBRAK cmd SEMICOLON cmd RBRAK { () }
  | LBRAK cmd PARALLEL cmd RBRAK  { () }
  | DECLARE VARIDT SEMICOLON cmd  { () }
  | MALLOC LPAREN VARIDT RPAREN   { () }
  | SKIP                          { () }
  | WHILE BOOL cmd                { () }
  | IF BOOL cmd ELSE cmd          { () }
  | ATOM LPAREN cmd RPAREN        { () }

slot :
    VARIDT              { () }
  | slot DOT FIELDIDT   { () }

assign :
    slot ASSIGN expr  { () }
	
expr :
    slot                              { () }
  | expr MINUS expr                   { () }
  | expr EQUAL expr                   { () }
  | expr LESSTHAN expr                { () }
  | LPAREN expr RPAREN                { () }
  | NUM                               { () }
  | PROC VARIDT COLUMN cmd            { () }
  
%% (* trailer *)
