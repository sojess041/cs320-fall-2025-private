{
open Parser
}

let ws     = [' ' '\t' '\r' '\n']+
let digit  = ['0'-'9']
let int    = '-'? digit+                               (* allow leading minus *)

(* first char: lowercase or underscore; rest: letters/digits/_/-/' *)
let ident_start = ['a'-'z' '_']
let ident_rest  = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '\'']
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
| "*"                     { STAR }
| "/"                     { SLASH }
| "("                     { LPAREN }
| ")"                     { RPAREN }
| "if"                    { IF }
| "then"                  { THEN }
| "else"                  { ELSE }
| "let"                   { LET }
| "in"                    { IN }
| "fun"                   { FUN }
| "true"                  { TRUE }
| "false"                 { FALSE }
| "mod"                   { MOD }
| int as n                { INT (int_of_string n) }    (* BEFORE "-" *)
| "-"                     { MINUS }                    (* AFTER int *)
| ident as s              { ID s }
| eof                     { EOF }
| _                       { failwith "lex error" }
