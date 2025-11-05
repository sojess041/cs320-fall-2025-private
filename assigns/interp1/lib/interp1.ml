(* lib/interp1.ml *)
open Utils
let ( let* ) r f = match r with Ok x -> f x | Error e -> Error e

(* You said parse is already working; keep your version or this one. *)
let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

(* ---------- helpers ---------- *)
let expr_of_value = function
  | VNum n        -> Num n
  | VBool true    -> True
  | VBool false   -> False
  | VUnit         -> Unit
  | VFun (x,body) -> Fun (x, body)

let as_int = function VNum n -> Some n | _ -> None
let as_bool = function VBool b -> Some b | _ -> None

(* ---------- substitution ---------- *)
let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ -> e
  | Var y -> if x = y then expr_of_value v else e
  | App (e1,e2) -> App (subst v x e1, subst v x e2)
  | Bop (op,e1,e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1,e2,e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y,e1,e2) ->
      if y = x then Let (y, subst v x e1, e2)
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y,body) ->
      if y = x then e else Fun (y, subst v x body)

(* ---------- evaluation (big-step, left-to-right) ---------- *)
let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit      -> Ok VUnit
  | True      -> Ok (VBool true)
  | False     -> Ok (VBool false)
  | Num n     -> Ok (VNum n)
  | Var x     -> Error (UnknownVar x)

  | If (c,t,f) ->
      (match eval c with
       | Error err -> Error err
       | Ok v ->
         match as_bool v with
         | None -> Error InvalidIfCond
         | Some true  -> eval t
         | Some false -> eval f)

  | Let (x,e1,e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 -> eval (subst v1 x e2))

  | Fun (x,body) -> Ok (VFun (x,body))

  | App (e1,e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok vf ->
         match vf with
         | VFun (x,body) ->
             (match eval e2 with
              | Error err -> Error err
              | Ok v2 -> eval (subst v2 x body))
         | _ -> Error InvalidApp)

  | Bop (op, e1, e2) ->
      let ( let* ) r f = match r with Ok x -> f x | Error e -> Error e in
      let* v1 = eval e1 in
      match op with
      | And ->
          (match as_bool v1 with
           | None -> Error (InvalidArgs And)
           | Some false -> Ok (VBool false)
           | Some true ->
               let* v2 = eval e2 in
               match as_bool v2 with
               | Some b -> Ok (VBool b)
               | None -> Error (InvalidArgs And))
      | Or  ->
          (match as_bool v1 with
           | None -> Error (InvalidArgs Or)
           | Some true -> Ok (VBool true)
           | Some false ->
               let* v2 = eval e2 in
               match as_bool v2 with
               | Some b -> Ok (VBool b)
               | None -> Error (InvalidArgs Or))
      | Add | Sub | Mul | Div | Mod
      | Lt  | Lte | Gt  | Gte | Eq  | Neq ->
          (match as_int v1 with
           | None -> Error (InvalidArgs op)
           | Some n1 ->
               let* v2 = eval e2 in
               match as_int v2 with
               | None -> Error (InvalidArgs op)
               | Some n2 ->
                 match op with
                 | Add -> Ok (VNum (n1 + n2))
                 | Sub -> Ok (VNum (n1 - n2))
                 | Mul -> Ok (VNum (n1 * n2))
                 | Div -> if n2 = 0 then Error DivByZero
                          else Ok (VNum (n1 / n2))
                 | Mod -> if n2 = 0 then Error DivByZero
                          else Ok (VNum (n1 mod n2))
                 | Lt  -> Ok (VBool (n1 <  n2))
                 | Lte -> Ok (VBool (n1 <= n2))
                 | Gt  -> Ok (VBool (n1 >  n2))
                 | Gte -> Ok (VBool (n1 >= n2))
                 | Eq  -> Ok (VBool (n1 =  n2))
                 | Neq -> Ok (VBool (n1 <> n2))
                 | And | Or -> assert false)


(* ---------- free-variable check for interp ---------- *)
let rec first_free (bound : string list) (e : expr) : string option =
  match e with
  | Unit | True | False | Num _ -> None
  | Var x -> if List.mem x bound then None else Some x
  | App (e1,e2)
  | Bop (_,e1,e2) ->
      (match first_free bound e1 with
       | Some x -> Some x
       | None -> first_free bound e2)
  | If (e1,e2,e3) ->
      (match first_free bound e1 with
       | Some x -> Some x
       | None ->
          match first_free bound e2 with
          | Some x -> Some x
          | None -> first_free bound e3)
  | Let (x,e1,e2) ->
      (match first_free bound e1 with
       | Some v -> Some v
       | None -> first_free (x::bound) e2)
  | Fun (x,body) -> first_free (x::bound) body

(* ---------- top-level ---------- *)
let interp (src : string) : (value, error) result =
  match parse src with
  | None -> Error ParseFail
  | Some e ->
      (match first_free [] e with
       | Some x -> Error (UnknownVar x)
       | None -> eval e)
