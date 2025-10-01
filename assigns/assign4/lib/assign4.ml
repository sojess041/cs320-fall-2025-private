type token = Lparen | Rparen | Atom of string
type sexpr = Atom of string | List of sexpr list

let is_space (c : char) : bool =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let tokens_of_string (s : string) : token list =
  let rec go (out : token list) (i : int) : token list =
    if i >= String.length s
    then List.rev out
    else if is_space s.[i]
    then go out (i + 1)
    else if s.[i] = '('
    then go (Lparen :: out) (i + 1)
    else if s.[i] = ')'
    then go (Rparen :: out) (i + 1)
    else go' out i (i + 1)
  and go' (out : token list) (i : int) (j : int) : token list =
    if j >= String.length s
       || is_space s.[j]
       || s.[j] = '('
       || s.[j] = ')'
    then go (Atom (String.sub s i (j - i)) :: out) j
    else go' out i (j + 1)
  in go [] 0


  let rec sexpr_of_tokens (toks : token list)
  : (sexpr * token list) option =
  match toks with
  | Atom s :: rest ->
      Some (Atom s, rest)

  | Lparen :: rest ->
      let (es, rest') = sexprs_of_tokens rest in
      begin match rest' with
      | Rparen :: rest'' -> Some (List es, rest'')
      | _ -> None
      end

  | _ -> None    


and sexprs_of_tokens (toks : token list)
  : sexpr list * token list =
  match toks with
  | Rparen :: _ -> ([], toks)         
  | [] -> ([], [])                    
  | _ ->
      match sexpr_of_tokens toks with
      | None -> ([], toks)            
      | Some (e, toks') ->
          let (es, rest) = sexprs_of_tokens toks' in
          (e :: es, rest)

let parse_sexpr (s : string) : sexpr option =
  match sexpr_of_tokens (tokens_of_string s) with
  | Some (e, []) -> Some e
  | _ -> None

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

  let int_of_string_opt s =
    try Some (int_of_string s) with _ -> None
  
let rec expr_of_sexpr (se : sexpr) : expr option =
  match se with
  | Atom s ->
      (match int_of_string_opt s with
        | Some n -> Some (Int n)
        | None -> None)
  | List (Atom op :: es) ->
      (match es with
        | [e1; e2] ->
            (match expr_of_sexpr e1, expr_of_sexpr e2 with
            | Some e1', Some e2' ->
                begin match op with
                | "+" -> Some (Add (e1', e2'))
                | "-" -> Some (Sub (e1', e2'))
                | "*" -> Some (Mul (e1', e2'))
                | "/" -> Some (Div (e1', e2'))
                | _ -> None
                end
            | _ -> None)
        | _ -> None)
  | List _ -> None
    
    

  let parse (s : string) : expr option =
    match parse_sexpr s with
    | Some se -> expr_of_sexpr se
    | None -> None
let rec eval (e : expr) : int =
  match e with
  | Int n     -> n
  | Add (a,b) -> eval a + eval b
  | Sub (a,b) -> eval a - eval b
  | Mul (a,b) -> eval a * eval b
  | Div (a,b) -> eval a / eval b   

let interp (s : string) : int option =
  match parse s with
  | Some e -> Some (eval e)
  | None -> None