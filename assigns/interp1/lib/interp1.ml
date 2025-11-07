include Utils
let parse (s : string) : expr option =
  try Some (Parser.prog Lexer.read (Lexing.from_string s))
  with _ -> None

let rec subst (v: value) (x: string) (e: expr) : expr =
  match e with
  | Num _ | True | False | Unit -> e
  | Var y -> 
      if x = y then 
        match v with 
        | VNum n -> Num n 
        | VBool b -> if b then True else False 
        | VUnit -> Unit 
        | VFun (param, body) -> Fun (param, body)
      else Var y
  | Bop (b, e1, e2) -> Bop (b, subst v x e1, subst v x e2)
  | If (cond, e1, e2) -> If (subst v x cond, subst v x e1, subst v x e2)
  | Let (y, e1, e2) ->
      if x = y then 
        Let (y, subst v x e1, e2)
      else 
        Let (y, subst v x e1, subst v x e2)
  | Fun (y, body) ->
      if x = y then 
        Fun (y, body)
      else 
        Fun (y, subst v x body)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)

let rec eval (e: expr) : (value, error) result =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | Bop (b, e1, e2) -> 
      let res1 = eval e1 in
      (match res1 with
       | Error err -> Error err
       | Ok v1 ->
           (match b, v1 with
            | And, VBool false -> Ok (VBool false)
            | Or, VBool true -> Ok (VBool true)
            | And, VBool true | Or, VBool false ->
                let res2 = eval e2 in
                (match res2 with
                 | Error err -> Error err
                 | Ok (VBool b2) -> Ok (VBool b2)
                 | Ok _ -> Error (InvalidArgs b))
            | _ ->
                let res2 = eval e2 in
                (match res2 with
                 | Error err -> Error err
                 | Ok v2 ->
                     (match b, v1, v2 with
                      | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
                      | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
                      | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
                      | Div, VNum n1, VNum n2 ->
                          if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
                      | Mod, VNum n1, VNum n2 ->
                          if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
                      | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
                      | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
                      | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
                      | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
                      | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
                      | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
                      | _, _, _ -> Error (InvalidArgs b)))))
  | If (cond, e1, e2) ->
      let res_cond = eval cond in
      (match res_cond with
       | Error err -> Error err
       | Ok (VBool true) -> eval e1
       | Ok (VBool false) -> eval e2
       | Ok _ -> Error InvalidIfCond)
  | Let (x, e1, e2) ->
      let res1 = eval e1 in
      (match res1 with
       | Error err -> Error err
       | Ok v -> eval (subst v x e2))
  | Fun (x, e) -> Ok (VFun (x, e))
  | App (e1, e2) ->
      let res1 = eval e1 in
      (match res1 with
       | Error err -> Error err
       | Ok (VFun (x, body)) ->
           let res2 = eval e2 in
           (match res2 with
            | Error err -> Error err
            | Ok v -> eval (subst v x body))
       | Ok _ -> Error InvalidApp)

let interp (input: string) : (value, error) result =
  match parse input with
  | None -> Error ParseFail
  | Some expr ->
      let rec find_free_vars expr bound_vars =
        match expr with
        | Num _ | True | False | Unit -> None
        | Var x -> if List.mem x bound_vars then None else Some x
        | Bop (_, e1, e2) -> 
            (match find_free_vars e1 bound_vars with
             | Some x -> Some x
             | None -> find_free_vars e2 bound_vars)
        | If (cond, e1, e2) ->
            (match find_free_vars cond bound_vars with
             | Some x -> Some x
             | None -> 
                 match find_free_vars e1 bound_vars with
                 | Some x -> Some x
                 | None -> find_free_vars e2 bound_vars)
        | Let (x, e1, e2) ->
            (match find_free_vars e1 bound_vars with
             | Some v -> Some v
             | None -> find_free_vars e2 (x :: bound_vars))
        | Fun (x, e) -> find_free_vars e (x :: bound_vars)
        | App (e1, e2) ->
            (match find_free_vars e1 bound_vars with
             | Some x -> Some x
             | None -> find_free_vars e2 bound_vars)
      in
      match find_free_vars expr [] with
      | Some x -> Error (UnknownVar x)
      | None -> eval expr
