
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | STAR
  | SLASH
  | RPAREN
  | PLUS
  | OR
  | NEQ
  | MOD
  | MINUS
  | LTE
  | LT
  | LPAREN
  | LET
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GTE
  | GT
  | FUN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Utils.prog)
