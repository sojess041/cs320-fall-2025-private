{
open Parser
}

let ws     = [' ' '\t' '\r' '\n']+
let digit  = ['0'-'9']
let int    = digit+
let letter = ['A'-'Z' 'a'-'z' '_']
let ident  = letter (letter | digit | '_')*

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
| "-"                     { MINUS }
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
| int as n                { INT (int_of_string n) }
| ident as s              { ID s }
| eof                     { EOF }
| _                       { failwith "lex error" }
