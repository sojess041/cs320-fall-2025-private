{
open Parser
}

(* Whitespace, numbers, and identifiers per spec *)

let ws     = [' ' '\t' '\r' '\n']+
let digit  = ['0'-'9']
let int    = '-'? digit+                           (* allow leading minus sign *)

(* first char: lowercase or underscore; rest: letters/digits/_/' *)
let ident_start = ['a'-'z' '_']
let ident_rest  = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'' ]
let ident       = ident_start ident_rest*

rule read = parse
  | ws                      { read lexbuf }
  | "->"                    { ARROW }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "<="                    { LTE }
  | ">="                    { GTE }
  | "<>"                    { NEQ }
  | "="                     { EQ }
  | "<"                     { LT }
  | ">"                     { GT }
  | "+"                     { PLUS }
  | "-"                     { MINUS }               (* must come after "->" but before numbers *)
  | "*"                     { STAR }
  | "/"                     { SLASH }
  | "mod"                   { MOD }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }
  | "let"                   { LET }
