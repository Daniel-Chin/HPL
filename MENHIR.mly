/* File MENHIR.mly */

%{ (* header *)
  
%} /* declarations */

/* lexer tokens */
%token EOL SEMICOLON ASSIGN MINUS DOT COLUMN EQUAL LESSTHAN
%token LPAREN RPAREN LBRAK RBRAK DECLARE MALLOC SKIP
%token WHILE IF ELSE PARALLEL ATOM
%token < string > VARIDT FIELDIDT
%token < int > NUM
%token < bool > BOOL
%start prog                   /* the entry point */
%type <unit> prog  
%type <unit> cmds
%type <unit> cmd
%type <unit> assign
%type <unit> expr
%type <unit> slot
%left MINUS LESSTHAN EQUAL

%% /* rules */

prog :
    cmds EOL  { () }
	
cmds :
    LBRAK cmd SEMICOLON cmds RBRAK  { () }
  | LBRAK cmd PARALLEL cmds RBRAK   { () }
  | cmd                             { () }
  
cmd :
    assign                        { () }
  | expr                          { () }
  | DECLARE VARIDT                { () }
  | MALLOC LPAREN VARIDT RPAREN   { () }
  | SKIP                          { () }
  | WHILE expr cmds               { () }
  | IF expr cmds ELSE cmds        { () }
  | ATOM LPAREN cmds RPAREN       { () }

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
  | BOOL                              { () }
  | VARIDT COLUMN LPAREN cmds RPAREN  { () }
  
%% (* trailer *)
