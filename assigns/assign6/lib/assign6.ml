
type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lte of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

let is_lowercase (c : char) : bool =
  c >= 'a' && c <= 'z'


type token = LPAR | RPAR | ATOM of string
let is_valid_var : string -> bool= String.for_all is_lowercase
let tokenize (s : string) : token list =
  let toks = ref [] in
  let buf  = Buffer.create 16 in
  let flush () =
    if Buffer.length buf > 0 then (
      toks := ATOM (Buffer.contents buf) :: !toks;
      Buffer.clear buf
    )
  in
  String.iter (fun c ->
    match c with
    | '(' -> flush (); toks := LPAR :: !toks
    | ')' -> flush (); toks := RPAR :: !toks
    | ' ' | '\t' | '\n' | '\r' -> flush ()
    | _ -> Buffer.add_char buf c
  ) s;
  flush ();
  List.rev !toks

let int_of_string_opt (s : string) =
  try Some (int_of_string s) with _ -> None

(* parse a binary form: (op e1 e2) *)
let rec parse_bin (ctor : expr * expr -> expr) (ts : token list)
  : (expr * token list) option =
  match parse_one ts with
  | Some (e1, ts1) ->
      (match parse_one ts1 with
       | Some (e2, RPAR :: rest) -> Some (ctor (e1, e2), rest)
       | _ -> None)
  | None -> None

(* parse one expression; returns (expr, remaining_tokens) *)
and parse_one (ts : token list) : (expr * token list) option =
  match ts with
  | ATOM "True"  :: rest -> Some (True,  rest)
  | ATOM "False" :: rest -> Some (False, rest)
  | ATOM a :: rest ->
      (match int_of_string_opt a with
       | Some n -> Some (Int n, rest)
       | None ->
           if is_valid_var a then Some (Var a, rest) else None)
  | LPAR :: ATOM "+"  :: ts' -> parse_bin (fun (x,y) -> Add (x,y)) ts'
  | LPAR :: ATOM "-"  :: ts' -> parse_bin (fun (x,y) -> Sub (x,y)) ts'
  | LPAR :: ATOM "*"  :: ts' -> parse_bin (fun (x,y) -> Mul (x,y)) ts'
  | LPAR :: ATOM "/"  :: ts' -> parse_bin (fun (x,y) -> Div (x,y)) ts'
  | LPAR :: ATOM "<=" :: ts' -> parse_bin (fun (x,y) -> Lte (x,y)) ts'
  | LPAR :: ATOM "If" :: ts' ->
      (match parse_one ts' with
       | Some (e1, ts1) ->
           (match parse_one ts1 with
            | Some (e2, ts2) ->
                (match parse_one ts2 with
                 | Some (e3, RPAR :: rest) -> Some (If (e1,e2,e3), rest)
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | LPAR :: ATOM "Let" :: ATOM x :: ts' when is_valid_var x ->
      (match parse_one ts' with
       | Some (e1, ts1) ->
           (match parse_one ts1 with
            | Some (e2, RPAR :: rest) -> Some (Let (x, e1, e2), rest)
            | _ -> None)
       | _ -> None)
  | _ -> None

let parse (s : string) : expr option =
  match parse_one (tokenize s) with
  | Some (e, []) -> Some e           
  | _ -> None

type ty =
  | IntTy
  | BoolTy

module M = Map.Make(String)
type ctxt = ty M.t

let empty_ctxt : ctxt = M.empty
let add_binding (x : string) (t : ty) (gamma : ctxt) : ctxt = M.add x t gamma
let check_binding (x : string) (gamma : ctxt) : ty option = M.find_opt x gamma

let rec type_of (gamma : ctxt) (e : expr) : ty option = 
  match e with
  | True  -> Some BoolTy
  | False -> Some BoolTy
  | Int _ -> Some IntTy
  | Var x ->
      check_binding x gamma

  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
      begin match type_of gamma e1, type_of gamma e2 with
      | Some IntTy, Some IntTy -> Some IntTy
      | _ -> None
      end

  | Lte (e1, e2) ->
      begin match type_of gamma e1, type_of gamma e2 with
      | Some IntTy, Some IntTy -> Some BoolTy
      | _ -> None
      end

  | If (e1, e2, e3) ->
      begin match type_of gamma e1 with
      | Some BoolTy ->
          begin match type_of gamma e2, type_of gamma e3 with
          | Some t2, Some t3 when t2 = t3 -> Some t2
          | _ -> None
          end
      | _ -> None
      end

  | Let (x, e1, e2) ->
      begin match type_of gamma e1 with
      | None -> None
      | Some t1 ->
          let gamma' = add_binding x t1 gamma in
          type_of gamma' e2
      end

let subst (e1 : expr) (x : string) (e2 : expr) : expr =
  let rec go e =
    match e with
    | True -> True
    | False -> False
    | Int n -> Int n
    | Var y -> if y = x then e1 else Var y
    | Add (e1, e2) -> Add (go e1, go e2)
    | Sub (e1, e2) -> Sub (go e1, go e2)
    | Mul (e1, e2) -> Mul (go e1, go e2)
    | Div (e1, e2) -> Div (go e1, go e2)
    | Lte (e1, e2) -> Lte (go e1, go e2)
    | If (e1, e2, e3) -> If (go e1, go e2, go e3)
    | Let (y, e1, e2) -> Let (y, go e1, if y = x then e2 else go e2)
  in go e2

type value =
  | IntV of int
  | BoolV of bool

  let expr_of_value (v : value) : expr =
    match v with
    | IntV n  -> Int n
    | BoolV b -> if b then True else False



    let rec eval (e : expr) : value =
      match e with
      | True        -> BoolV true
      | False       -> BoolV false
      | Int n       -> IntV n
      | Var _       -> assert false  
      | Add (e1,e2) ->
          (match eval e1, eval e2 with
           | IntV a, IntV b -> IntV (a + b)
           | _ -> assert false)
      | Sub (e1,e2) ->
          (match eval e1, eval e2 with
           | IntV a, IntV b -> IntV (a - b)
           | _ -> assert false)
      | Mul (e1,e2) ->
          (match eval e1, eval e2 with
           | IntV a, IntV b -> IntV (a * b)
           | _ -> assert false)
      | Div (e1,e2) ->
          (match eval e1, eval e2 with
           | IntV a, IntV b -> IntV (a / b)
           | _ -> assert false)
      | Lte (e1,e2) ->
          (match eval e1, eval e2 with
           | IntV a, IntV b -> BoolV (a <= b)
           | _ -> assert false)
      | If (e1,e2,e3) ->
          (match eval e1 with
           | BoolV true  -> eval e2
           | BoolV false -> eval e3
           | _ -> assert false)
      | Let (x,e1,e2) ->
          let v1  = eval e1 in
          let e1' = expr_of_value v1 in
          eval (subst e1' x e2)   
let interp ?(print=true) (s : string) : value option =
  let print s = if print then print_endline s else () in
  let _ = print "parsing..." in
  match parse s with
  | None -> None
  | Some e ->
    let _ = print "type_checking..." in
    match type_of empty_ctxt e with
    | None -> None
    | Some _ ->
      let _ = print "evaluating..." in
      Some (eval e)
