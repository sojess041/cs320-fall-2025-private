%{
(* Build the AST defined in Utils *)
open Utils
%}

%token IF THEN ELSE LET IN FUN ARROW
%token TRUE FALSE
%token PLUS MINUS STAR SLASH MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR
%token LPAREN RPAREN
%token <int> INT
%token <string> ID
%token EOF

%start <Utils.prog> prog
%%

(* A program is one expression followed by EOF *)
prog:
  | expr EOF                               { $1 }
;

(* Highest-level expression forms *)
expr:
  | IF expr THEN expr ELSE expr            { If ($2, $4, $6) }
  | LET ID EQ expr IN expr                 { Let ($2, $4, $6) }
  | FUN ID ARROW expr                      { Fun ($2, $4) }
  | disj                                   { $1 }
;

(* || is RIGHT-associative *)
disj:
  | conj OR disj                           { Bop (Or,  $1, $3) }
  | conj                                   { $1 }
;

(* && is RIGHT-associative *)
conj:
  | cmp AND conj                           { Bop (And, $1, $3) }
  | cmp                                    { $1 }
;

(* Comparisons are LEFT-associative *)
cmp:
  | cmp LT  add                            { Bop (Lt,  $1, $3) }
  | cmp LTE add                            { Bop (Lte, $1, $3) }
  | cmp GT  add                            { Bop (Gt,  $1, $3) }
  | cmp GTE add                            { Bop (Gte, $1, $3) }
  | cmp EQ  add                            { Bop (Eq,  $1, $3) }
  | cmp NEQ add                            { Bop (Neq, $1, $3) }
  | add                                    { $1 }
;

(* + and - are LEFT-associative *)
add:
  | add PLUS  mul                          { Bop (Add, $1, $3) }
  | add MINUS mul                          { Bop (Sub, $1, $3) }
  | mul                                    { $1 }
;

(* *, /, mod are LEFT-associative *)
mul:
  | mul STAR  app                          { Bop (Mul, $1, $3) }
  | mul SLASH app                          { Bop (Div, $1, $3) }
  | mul MOD   app                          { Bop (Mod, $1, $3) }
  | app                                    { $1 }
;

(* Function application is LEFT-associative and binds tighter than ops *)
app:
  | app atom                               { App ($1, $2) }
  | atom                                   { $1 }
;

(* Atoms *)
atom:
  | INT                                    { Num $1 }
  | TRUE                                   { True }
  | FALSE                                  { False }
  | ID                                     { Var $1 }
  | LPAREN RPAREN                          { Unit }
  | LPAREN expr RPAREN                     { $2 }
;
