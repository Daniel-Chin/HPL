{
open MENHIR;; (* Type token defined in MENHIR.mli *)
exception Eof;;
}
rule token = parse
    [' ' '\t' '\n'] 
                { token lexbuf }
  | "null"      { NULL }
  | "true"      { BOOL (true) }
  | "false"     { BOOL (false) }
  | "proc"      { PROC }
  | "var"       { DECLARE }
  | "malloc"    { MALLOC }
  | "skip"      { SKIP }
  | "while"     { WHILE }
  | "if"        { IF }
  | "else"      { ELSE }
  | "atom"      { ATOM }
  | "THE_END"   { THE_END }
  | ['A'-'Z'] (['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as idt
                { VARIDT (idt) }
  | ['a'-'z'] (['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as idt
                { FIELDIDT (idt) }
  | ['0'-'9']+ as num
                { NUM (int_of_string num) }
  | "|||"       { PARALLEL }
  | ';'         { SEMICOLON }
  | '='         { ASSIGN }
  | '-'         { MINUS }
  | '.'         { DOT }
  | ':'         { COLUMN }
  | "=="        { EQUAL }
  | '<'         { LESSTHAN }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRAK }
  | '}'         { RBRAK }
  | eof         { raise Eof }
