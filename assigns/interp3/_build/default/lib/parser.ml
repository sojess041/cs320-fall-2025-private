
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WITH
    | VAR of (
# 27 "lib/parser.mly"
       (string)
# 16 "lib/parser.ml"
  )
    | TVAR of (
# 55 "lib/parser.mly"
       (string)
# 21 "lib/parser.ml"
  )
    | TUNIT
    | TRUE
    | TOPTION
    | TLIST
    | TINT
    | THEN
    | TFLOAT
    | TBOOL
    | SUBF
    | SUB
    | STAR
    | SOME
    | SEMICOLON
    | RPAREN
    | REC
    | RBRACKET
    | POW
    | OR
    | NONE
    | NEQ
    | MULF
    | MOD
    | MATCH
    | LTE
    | LT
    | LPAREN
    | LET
    | LBRACKET
    | INT of (
# 25 "lib/parser.mly"
       (int)
# 54 "lib/parser.ml"
  )
    | IN
    | IF
    | GTE
    | GT
    | FUN
    | FLOAT of (
# 26 "lib/parser.mly"
       (float)
# 64 "lib/parser.ml"
  )
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DIVF
    | DIV
    | CONS
    | COMMA
    | COLON
    | ASSERT
    | ARROW
    | AND
    | ALT
    | ADDF
    | ADD
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
open Utils

let mk_func ty args body =
  let body =
    match ty with
    | None -> body
    | Some ty -> Annot (body, ty)
  in
  List.fold_right
    (fun (x, ty) acc -> Fun (x, ty, acc))
    args
    body

let mk_list h es =
  let tl = List.fold_right
    (fun x acc -> Bop (Cons, x, acc))
    es
    Nil
  in
  Bop (Cons, h, tl)

# 109 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_prog) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState001 : (('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_state
    (** State 001.
        Stack shape : LET.
        Start symbol: prog. *)

  | MenhirState004 : ((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 004.
        Stack shape : LET option(REC) VAR.
        Start symbol: prog. *)

  | MenhirState007 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 007.
        Stack shape : LPAREN VAR.
        Start symbol: prog. *)

  | MenhirState008 : (('s, _menhir_box_prog) _menhir_cell1_COLON, _menhir_box_prog) _menhir_state
    (** State 008.
        Stack shape : COLON.
        Start symbol: prog. *)

  | MenhirState014 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 014.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState019 : (('s, _menhir_box_prog) _menhir_cell1_ty, _menhir_box_prog) _menhir_state
    (** State 019.
        Stack shape : ty.
        Start symbol: prog. *)

  | MenhirState024 : (((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_state
    (** State 024.
        Stack shape : LET option(REC) VAR list(arg).
        Start symbol: prog. *)

  | MenhirState026 : ((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_, _menhir_box_prog) _menhir_state
    (** State 026.
        Stack shape : LET option(REC) VAR list(arg) option(annot).
        Start symbol: prog. *)

  | MenhirState029 : (('s, _menhir_box_prog) _menhir_cell1_SOME, _menhir_box_prog) _menhir_state
    (** State 029.
        Stack shape : SOME.
        Start symbol: prog. *)

  | MenhirState031 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 031.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState033 : (('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_state
    (** State 033.
        Stack shape : LET.
        Start symbol: prog. *)

  | MenhirState035 : ((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 035.
        Stack shape : LET option(REC) VAR.
        Start symbol: prog. *)

  | MenhirState036 : (((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_state
    (** State 036.
        Stack shape : LET option(REC) VAR list(arg).
        Start symbol: prog. *)

  | MenhirState038 : ((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_, _menhir_box_prog) _menhir_state
    (** State 038.
        Stack shape : LET option(REC) VAR list(arg) option(annot).
        Start symbol: prog. *)

  | MenhirState039 : (('s, _menhir_box_prog) _menhir_cell1_LBRACKET, _menhir_box_prog) _menhir_state
    (** State 039.
        Stack shape : LBRACKET.
        Start symbol: prog. *)

  | MenhirState042 : (('s, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_state
    (** State 042.
        Stack shape : FUN.
        Start symbol: prog. *)

  | MenhirState044 : ((('s, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_state
    (** State 044.
        Stack shape : FUN list(arg).
        Start symbol: prog. *)

  | MenhirState047 : (('s, _menhir_box_prog) _menhir_cell1_ASSERT, _menhir_box_prog) _menhir_state
    (** State 047.
        Stack shape : ASSERT.
        Start symbol: prog. *)

  | MenhirState050 : (('s, _menhir_box_prog) _menhir_cell1_expr3, _menhir_box_prog) _menhir_state
    (** State 050.
        Stack shape : expr3.
        Start symbol: prog. *)

  | MenhirState053 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 053.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState055 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 055.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState057 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 057.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState059 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 059.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState061 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 061.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState063 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 063.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState065 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 065.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState067 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 067.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState069 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 069.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState071 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 071.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState073 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 073.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState075 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 075.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState077 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 077.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState079 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 079.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState081 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 081.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState083 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 083.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState085 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 085.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState087 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 087.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState089 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 089.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState091 : (('s, _menhir_box_prog) _menhir_cell1_expr2, _menhir_box_prog) _menhir_state
    (** State 091.
        Stack shape : expr2.
        Start symbol: prog. *)

  | MenhirState094 : (('s, _menhir_box_prog) _menhir_cell1_arg, _menhir_box_prog) _menhir_state
    (** State 094.
        Stack shape : arg.
        Start symbol: prog. *)

  | MenhirState096 : ((('s, _menhir_box_prog) _menhir_cell1_LBRACKET, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 096.
        Stack shape : LBRACKET expr.
        Start symbol: prog. *)

  | MenhirState097 : (('s, _menhir_box_prog) _menhir_cell1_SEMICOLON, _menhir_box_prog) _menhir_state
    (** State 097.
        Stack shape : SEMICOLON.
        Start symbol: prog. *)

  | MenhirState099 : (('s, _menhir_box_prog) _menhir_cell1_list_item, _menhir_box_prog) _menhir_state
    (** State 099.
        Stack shape : list_item.
        Start symbol: prog. *)

  | MenhirState104 : (((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 104.
        Stack shape : LET option(REC) VAR list(arg) option(annot) expr.
        Start symbol: prog. *)

  | MenhirState109 : ((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 109.
        Stack shape : LPAREN expr.
        Start symbol: prog. *)

  | MenhirState114 : (('s, _menhir_box_prog) _menhir_cell1_toplet, _menhir_box_prog) _menhir_state
    (** State 114.
        Stack shape : toplet.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_arg = 
  | MenhirCell1_arg of 's * ('s, 'r) _menhir_state * (string * Utils.ty option)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Utils.expr)

and ('s, 'r) _menhir_cell1_expr2 = 
  | MenhirCell1_expr2 of 's * ('s, 'r) _menhir_state * (Utils.expr)

and ('s, 'r) _menhir_cell1_expr3 = 
  | MenhirCell1_expr3 of 's * ('s, 'r) _menhir_state * (Utils.expr)

and ('s, 'r) _menhir_cell1_list_arg_ = 
  | MenhirCell1_list_arg_ of 's * ('s, 'r) _menhir_state * ((string * Utils.ty option) list)

and ('s, 'r) _menhir_cell1_list_item = 
  | MenhirCell1_list_item of 's * ('s, 'r) _menhir_state * (Utils.expr)

and ('s, 'r) _menhir_cell1_option_REC_ = 
  | MenhirCell1_option_REC_ of 's * ('s, 'r) _menhir_state * (unit option)

and ('s, 'r) _menhir_cell1_option_annot_ = 
  | MenhirCell1_option_annot_ of 's * ('s, 'r) _menhir_state * (Utils.ty option)

and ('s, 'r) _menhir_cell1_toplet = 
  | MenhirCell1_toplet of 's * ('s, 'r) _menhir_state * (Utils.toplet)

and ('s, 'r) _menhir_cell1_ty = 
  | MenhirCell1_ty of 's * ('s, 'r) _menhir_state * (Utils.ty)

and ('s, 'r) _menhir_cell1_ASSERT = 
  | MenhirCell1_ASSERT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COLON = 
  | MenhirCell1_COLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACKET = 
  | MenhirCell1_LBRACKET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SEMICOLON = 
  | MenhirCell1_SEMICOLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SOME = 
  | MenhirCell1_SOME of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 27 "lib/parser.mly"
       (string)
# 406 "lib/parser.ml"
)

and _menhir_box_prog = 
  | MenhirBox_prog of (Utils.prog) [@@unboxed]

let _menhir_action_01 =
  fun ty ->
    (
# 114 "lib/parser.mly"
                 ( ty )
# 417 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_02 =
  fun x ->
    (
# 132 "lib/parser.mly"
                                      ( (x, None) )
# 425 "lib/parser.ml"
     : (string * Utils.ty option))

let _menhir_action_03 =
  fun ty x ->
    (
# 133 "lib/parser.mly"
                                      ( (x, Some ty) )
# 433 "lib/parser.ml"
     : (string * Utils.ty option))

let _menhir_action_04 =
  fun args binding body name rc ty ->
    (
# 139 "lib/parser.mly"
    ( Let
        {
          is_rec = Option.is_some rc;
          name;
          binding = mk_func ty args binding;
          body;
        }
    )
# 448 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_05 =
  fun args body ->
    (
# 148 "lib/parser.mly"
    ( mk_func None args body )
# 456 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_06 =
  fun e ->
    (
# 150 "lib/parser.mly"
              ( e )
# 464 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_07 =
  fun e1 e2 ->
    let op = 
# 153 "lib/parser.mly"
         ( Add )
# 472 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 477 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_08 =
  fun e1 e2 ->
    let op = 
# 154 "lib/parser.mly"
         ( Sub )
# 485 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 490 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_09 =
  fun e1 e2 ->
    let op = 
# 155 "lib/parser.mly"
         ( Mul )
# 498 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 503 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_10 =
  fun e1 e2 ->
    let op = 
# 156 "lib/parser.mly"
         ( Div )
# 511 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 516 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_11 =
  fun e1 e2 ->
    let op = 
# 157 "lib/parser.mly"
         ( Mod )
# 524 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 529 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_12 =
  fun e1 e2 ->
    let op = 
# 158 "lib/parser.mly"
         ( AddF )
# 537 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 542 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_13 =
  fun e1 e2 ->
    let op = 
# 159 "lib/parser.mly"
         ( SubF )
# 550 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 555 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_14 =
  fun e1 e2 ->
    let op = 
# 160 "lib/parser.mly"
         ( MulF )
# 563 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 568 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_15 =
  fun e1 e2 ->
    let op = 
# 161 "lib/parser.mly"
         ( DivF )
# 576 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 581 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_16 =
  fun e1 e2 ->
    let op = 
# 162 "lib/parser.mly"
         ( PowF )
# 589 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 594 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_17 =
  fun e1 e2 ->
    let op = 
# 163 "lib/parser.mly"
         ( Cons )
# 602 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 607 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_18 =
  fun e1 e2 ->
    let op = 
# 164 "lib/parser.mly"
         ( Lt )
# 615 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 620 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_19 =
  fun e1 e2 ->
    let op = 
# 165 "lib/parser.mly"
         ( Lte )
# 628 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 633 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_20 =
  fun e1 e2 ->
    let op = 
# 166 "lib/parser.mly"
         ( Gt )
# 641 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 646 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_21 =
  fun e1 e2 ->
    let op = 
# 167 "lib/parser.mly"
         ( Lte )
# 654 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 659 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_22 =
  fun e1 e2 ->
    let op = 
# 168 "lib/parser.mly"
         ( Eq )
# 667 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 672 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_23 =
  fun e1 e2 ->
    let op = 
# 169 "lib/parser.mly"
         ( Neq )
# 680 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 685 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_24 =
  fun e1 e2 ->
    let op = 
# 170 "lib/parser.mly"
         ( And )
# 693 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 698 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_25 =
  fun e1 e2 ->
    let op = 
# 171 "lib/parser.mly"
         ( Or )
# 706 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 711 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_26 =
  fun e1 e2 ->
    let op = 
# 172 "lib/parser.mly"
          ( Comma )
# 719 "lib/parser.ml"
     in
    (
# 175 "lib/parser.mly"
                                     ( Bop (op, e1, e2) )
# 724 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_27 =
  fun e ->
    (
# 176 "lib/parser.mly"
                                     ( Assert e )
# 732 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_28 =
  fun e ->
    (
# 177 "lib/parser.mly"
                                     ( ESome e )
# 740 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_29 =
  fun es ->
    (
# 179 "lib/parser.mly"
    ( List.(fold_left
              (fun acc x -> App (acc, x))
              (hd es)
              (tl es))
    )
# 752 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_30 =
  fun () ->
    (
# 189 "lib/parser.mly"
                                            ( Unit )
# 760 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_31 =
  fun () ->
    (
# 190 "lib/parser.mly"
                                            ( Bool true )
# 768 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_32 =
  fun () ->
    (
# 191 "lib/parser.mly"
                                            ( Bool false )
# 776 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_33 =
  fun () ->
    (
# 192 "lib/parser.mly"
                                            ( ENone )
# 784 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_34 =
  fun () ->
    (
# 193 "lib/parser.mly"
                                            ( Nil )
# 792 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_35 =
  fun e es ->
    (
# 195 "lib/parser.mly"
      ( mk_list e es )
# 800 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_36 =
  fun n ->
    (
# 196 "lib/parser.mly"
                                            ( Int n )
# 808 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_37 =
  fun n ->
    (
# 197 "lib/parser.mly"
                                            ( Float n )
# 816 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_38 =
  fun x ->
    (
# 198 "lib/parser.mly"
                                            ( Var x )
# 824 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_39 =
  fun e ->
    (
# 199 "lib/parser.mly"
                                            ( e )
# 832 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_40 =
  fun e t ->
    (
# 200 "lib/parser.mly"
                                            ( Annot (e, t) )
# 840 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_41 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 848 "lib/parser.ml"
     : ((string * Utils.ty option) list))

let _menhir_action_42 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 856 "lib/parser.ml"
     : ((string * Utils.ty option) list))

let _menhir_action_43 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 864 "lib/parser.ml"
     : (Utils.expr list))

let _menhir_action_44 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 872 "lib/parser.ml"
     : (Utils.expr list))

let _menhir_action_45 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 880 "lib/parser.ml"
     : (Utils.prog))

let _menhir_action_46 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 888 "lib/parser.ml"
     : (Utils.prog))

let _menhir_action_47 =
  fun e ->
    (
# 186 "lib/parser.mly"
                      ( e )
# 896 "lib/parser.ml"
     : (Utils.expr))

let _menhir_action_48 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 904 "lib/parser.ml"
     : (Utils.expr list))

let _menhir_action_49 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 912 "lib/parser.ml"
     : (Utils.expr list))

let _menhir_action_50 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 920 "lib/parser.ml"
     : (unit option))

let _menhir_action_51 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 928 "lib/parser.ml"
     : (unit option))

let _menhir_action_52 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 936 "lib/parser.ml"
     : (Utils.ty option))

let _menhir_action_53 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 944 "lib/parser.ml"
     : (Utils.ty option))

let _menhir_action_54 =
  fun ls ->
    (
# 102 "lib/parser.mly"
                     ( ls )
# 952 "lib/parser.ml"
     : (Utils.prog))

let _menhir_action_55 =
  fun args binding name rc ty ->
    (
# 106 "lib/parser.mly"
    ( {
        is_rec = Option.is_some rc;
        name;
        binding = mk_func ty args binding;
      }
    )
# 965 "lib/parser.ml"
     : (Utils.toplet))

let _menhir_action_56 =
  fun () ->
    (
# 119 "lib/parser.mly"
                                 ( TUnit )
# 973 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_57 =
  fun () ->
    (
# 120 "lib/parser.mly"
                                 ( TInt )
# 981 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_58 =
  fun () ->
    (
# 121 "lib/parser.mly"
                                 ( TFloat )
# 989 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_59 =
  fun () ->
    (
# 122 "lib/parser.mly"
                                 ( TBool )
# 997 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_60 =
  fun a ->
    (
# 123 "lib/parser.mly"
                                 ( TVar a )
# 1005 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_61 =
  fun t ->
    (
# 124 "lib/parser.mly"
                                 ( t )
# 1013 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_62 =
  fun t ->
    (
# 125 "lib/parser.mly"
                                 ( TList t )
# 1021 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_63 =
  fun t ->
    (
# 126 "lib/parser.mly"
                                 ( TOption t )
# 1029 "lib/parser.ml"
     : (Utils.ty))

let _menhir_action_64 =
  fun t1 t2 ->
    (
# 127 "lib/parser.mly"
                                 ( TFun (t1, t2) )
# 1037 "lib/parser.ml"
     : (Utils.ty))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | ADDF ->
        "ADDF"
    | ALT ->
        "ALT"
    | AND ->
        "AND"
    | ARROW ->
        "ARROW"
    | ASSERT ->
        "ASSERT"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | CONS ->
        "CONS"
    | DIV ->
        "DIV"
    | DIVF ->
        "DIVF"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FLOAT _ ->
        "FLOAT"
    | FUN ->
        "FUN"
    | GT ->
        "GT"
    | GTE ->
        "GTE"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LBRACKET ->
        "LBRACKET"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | LTE ->
        "LTE"
    | MATCH ->
        "MATCH"
    | MOD ->
        "MOD"
    | MULF ->
        "MULF"
    | NEQ ->
        "NEQ"
    | NONE ->
        "NONE"
    | OR ->
        "OR"
    | POW ->
        "POW"
    | RBRACKET ->
        "RBRACKET"
    | REC ->
        "REC"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | SOME ->
        "SOME"
    | STAR ->
        "STAR"
    | SUB ->
        "SUB"
    | SUBF ->
        "SUBF"
    | TBOOL ->
        "TBOOL"
    | TFLOAT ->
        "TFLOAT"
    | THEN ->
        "THEN"
    | TINT ->
        "TINT"
    | TLIST ->
        "TLIST"
    | TOPTION ->
        "TOPTION"
    | TRUE ->
        "TRUE"
    | TUNIT ->
        "TUNIT"
    | TVAR _ ->
        "TVAR"
    | VAR _ ->
        "VAR"
    | WITH ->
        "WITH"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_117 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let ls = _v in
      let _v = _menhir_action_54 ls in
      MenhirBox_prog _v
  
  let rec _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_toplet -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let MenhirCell1_toplet (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_46 x xs in
      _menhir_goto_list_toplet_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_toplet_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_117 _menhir_stack _v
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState001
      | VAR _ ->
          let _v = _menhir_action_50 () in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState001 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = () in
      let _v = _menhir_action_51 x in
      _menhir_goto_option_REC_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_option_REC_ : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState001 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_034 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_REC_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_1 ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState035
          | LPAREN ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
          | COLON | EQ ->
              let _v_2 = _menhir_action_41 () in
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState035 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_02 x in
      _menhir_goto_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState094
      | LPAREN ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | ARROW | COLON | EQ ->
          let _v_1 = _menhir_action_41 () in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState007 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COLON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState008 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TFLOAT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let a = _v in
      let _v = _menhir_action_60 a in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_110 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TOPTION ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TLIST ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_40 e t in
          _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ARROW ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ty -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let _v = _menhir_action_63 t in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ty -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let _v = _menhir_action_62 t in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr3 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState029 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState026 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_112 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_SOME -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SOME (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_28 e in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr2 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | EOF | IN | LET | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_26 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState053 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_38 x in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_31 () in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SOME (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_33 () in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | TRUE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | SOME ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_30 () in
          _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NONE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LET ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | INT _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | FUN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | FLOAT _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | FALSE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | ASSERT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | VAR _ ->
          let _v = _menhir_action_50 () in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039
      | TRUE ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | SOME ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_34 () in
          _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NONE ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | LET ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | INT _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039
      | FUN ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | FLOAT _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039
      | FALSE ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | ASSERT ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let n = _v in
      let _v = _menhir_action_36 n in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState042
      | LPAREN ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | ARROW ->
          let _v = _menhir_action_41 () in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState042 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_arg_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState044 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRUE ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FLOAT _v ->
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ASSERT ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let n = _v in
      let _v = _menhir_action_37 n in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_32 () in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ASSERT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState047 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState067 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState057 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState069 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState071 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState059 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState079 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState081 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState083 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState085 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState087 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState063 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState065 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState089 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState075 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState077 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | COMMA | EOF | IN | LET | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_24 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_22 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_20 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_21 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_18 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_07 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_12 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_17 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COLON | COMMA | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_23 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | COMMA | EOF | IN | LET | RBRACKET | RPAREN | SEMICOLON ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_25 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_08 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_10 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_15 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_11 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_14 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_16 e1 e2 in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_09 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr2 as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ADDF | AND | COLON | COMMA | CONS | EOF | EQ | GT | GTE | IN | LET | LT | LTE | NEQ | OR | RBRACKET | RPAREN | SEMICOLON | SUB | SUBF ->
          let MenhirCell1_expr2 (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUBF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUB ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GTE ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONS ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState091 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRUE ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ASSERT ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADDF ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | EOF | IN | LET | RBRACKET | RPAREN | SEMICOLON ->
          let e = _v in
          let _v = _menhir_action_06 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState026 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState031 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState038 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState039 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_113 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_option_annot_ (_menhir_stack, _, ty) = _menhir_stack in
      let MenhirCell1_list_arg_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, name) = _menhir_stack in
      let MenhirCell1_option_REC_ (_menhir_stack, _, rc) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let binding = _v in
      let _v = _menhir_action_55 args binding name rc ty in
      let _menhir_stack = MenhirCell1_toplet (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LET ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | EOF ->
          let _v_0 = _menhir_action_45 () in
          _menhir_run_115 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_39 e in
          _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState109 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR _v ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TINT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TFLOAT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_56 () in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_57 () in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_58 () in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_59 () in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState014 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TFLOAT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, binding) = _menhir_stack in
      let MenhirCell1_option_annot_ (_menhir_stack, _, ty) = _menhir_stack in
      let MenhirCell1_list_arg_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, name) = _menhir_stack in
      let MenhirCell1_option_REC_ (_menhir_stack, _, rc) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let body = _v in
      let _v = _menhir_action_04 args binding body name rc ty in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_103 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_, _menhir_box_prog) _menhir_cell1_option_annot_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState104 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRUE ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FLOAT _v ->
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ASSERT ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_SEMICOLON -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SEMICOLON (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_47 e in
      let _menhir_stack = MenhirCell1_list_item (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | RBRACKET ->
          let _v_0 = _menhir_action_43 () in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SEMICOLON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FLOAT _v ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASSERT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_list_item -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_list_item (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_44 x xs in
      _menhir_goto_list_list_item_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_list_item_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState096 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LBRACKET, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) = _menhir_stack in
      let es = _v in
      let _v = _menhir_action_35 e es in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_096 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LBRACKET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
      | RBRACKET ->
          let _v_0 = _menhir_action_43 () in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_cell1_list_arg_ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_list_arg_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let body = _v in
      let _v = _menhir_action_05 args body in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState050
      | TRUE ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | NONE ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | LPAREN ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState050
      | FLOAT _v_2 ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState050
      | FALSE ->
          let _menhir_stack = MenhirCell1_expr3 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | ADD | ADDF | AND | COLON | COMMA | CONS | DIV | DIVF | EOF | EQ | GT | GTE | IN | LET | LT | LTE | MOD | MULF | NEQ | OR | POW | RBRACKET | RPAREN | SEMICOLON | STAR | SUB | SUBF ->
          let x = _v in
          let _v = _menhir_action_48 x in
          _menhir_goto_nonempty_list_expr3_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_expr3_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState050 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState026 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr3 -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr3 (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_49 x xs in
      _menhir_goto_nonempty_list_expr3_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_049 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let es = _v in
      let _v = _menhir_action_29 es in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ASSERT -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ASSERT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_27 e in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_019 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ty -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState019 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TFLOAT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_COLON as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TOPTION ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TLIST ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ | RPAREN ->
          let MenhirCell1_COLON (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_01 ty in
          _menhir_goto_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_annot : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState024 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_106 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let x = _v in
      let _v = _menhir_action_53 x in
      _menhir_goto_option_annot_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_option_annot_ : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState036 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_037 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_annot_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState038 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRUE ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FLOAT _v ->
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ASSERT ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_list_arg_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_annot_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState026 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRUE ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FLOAT _v ->
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ASSERT ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_VAR (_menhir_stack, x) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_03 ty x in
          _menhir_goto_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ty as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ | RPAREN | TLIST | TOPTION ->
          let MenhirCell1_ty (_menhir_stack, _menhir_s, t1) = _menhir_stack in
          let t2 = _v in
          let _v = _menhir_action_64 t1 t2 in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TOPTION ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TLIST ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_61 t in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ARROW ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_095 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_arg -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_arg (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_42 x xs in
      _menhir_goto_list_arg_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_arg_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_036 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_arg_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | EQ ->
          let _v_0 = _menhir_action_52 () in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState036 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_option_REC_ _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_arg_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | EQ ->
          let _v_0 = _menhir_action_52 () in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState024 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_REC_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_1 ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState004
          | LPAREN ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
          | COLON | EQ ->
              let _v_2 = _menhir_action_41 () in
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState004 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LET ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_45 () in
          _menhir_run_117 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
