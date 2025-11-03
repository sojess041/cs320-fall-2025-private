{
open Parser
}

(* whitespace, numbers, and identifiers per spec *)

let ws     = [' ' '\t' '\r' '\n']+
let int    = '-'? ['0'-'9']+             (* allow leading minus *)

(* var must start with lowercase letter or underscore; rest alnum/_/' *)
let ident  = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | ws                      { read lexbuf }           (* skip whitespace *)

  (* multi-char operators/keywords first *)
  | "->"                    { ARROW }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "<="                    { LTE }
  | ">="                    { GTE }
  | "<>"                    { NEQ }

  (* single-char symbols *)
  | "="                     { EQ }
  | "<"                     { LT }
  | ">"                     { GT }
  | "+"                     { PLUS }
  | "-"                     { MINUS }                 (* after "->" *)
  | "*"                     { STAR }
  | "/"                     { SLASH }
  | "mod"                   { MOD }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }

  (* keywords *)
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }
  | "let"                   { LET }
  | "in"                    { IN }
  | "fun"                   { FUN }
  | "true"                  { TRUE }
  | "false"                 { FALSE }

  (* literals and identifiers *)
  | int as n                { INT (int_of_string n) }
  | ident as s              { ID s }

  | eof                     { EOF }
  | _                       { failwith "lex error" }
