include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

type subst = (ident * ty) list

let _ = (fun (x : subst option) -> x)

let rec subst_ty a t ty0 =
  match ty0 with
  | TUnit | TInt | TFloat | TBool -> ty0
  | TVar b -> if b = a then t else ty0
  | TList t1 -> TList (subst_ty a t t1)
  | TOption t1 -> TOption (subst_ty a t t1)
  | TPair (t1, t2) -> TPair (subst_ty a t t1, subst_ty a t t2)
  | TFun (t1, t2) -> TFun (subst_ty a t t1, subst_ty a t t2)

let apply_subst_ty (s : subst) (ty0 : ty) : ty =
  List.fold_left (fun acc (a, t) -> subst_ty a t acc) ty0 s

let rec occurs a t =
  match t with
  | TUnit | TInt | TFloat | TBool -> false
  | TVar b -> a = b
  | TList t1 | TOption t1 -> occurs a t1
  | TPair (t1, t2) | TFun (t1, t2) ->
      occurs a t1 || occurs a t2

let rec free_ty_vars t =
  match t with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar a -> VarSet.singleton a
  | TList t1 | TOption t1 -> free_ty_vars t1
  | TPair (t1, t2) | TFun (t1, t2) ->
      VarSet.union (free_ty_vars t1) (free_ty_vars t2)

let rec unify (cs : constr list) (s : subst) : subst option =
  match cs with
  | [] -> Some s
  | (t1, t2) :: rest ->
      let t1 = apply_subst_ty s t1 in
      let t2 = apply_subst_ty s t2 in
      match t1, t2 with
      | TUnit, TUnit
      | TInt, TInt
      | TFloat, TFloat
      | TBool, TBool ->
          unify rest s

      | TList a, TList b
      | TOption a, TOption b ->
          unify ((a, b) :: rest) s

      | TPair (a1, b1), TPair (a2, b2)
      | TFun (a1, b1), TFun (a2, b2) ->
          unify ((a1, a2) :: (b1, b2) :: rest) s

      | TVar a, TVar b when a = b ->
          unify rest s

      | TVar a, t
      | t, TVar a ->
          if occurs a t then None
          else unify rest (s @ [ (a, t) ])

      | _ -> None

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify cs [] with
  | None -> None
  | Some s ->
      let ty' = apply_subst_ty s ty in
      let vars = free_ty_vars ty' in
      Some (Forall (vars, ty'))

exception Type_error

let type_of (ctxt : stc_env) (e : expr) : ty_scheme option =
  let fresh () = gensym () in

  let instantiate (Forall (vars, ty)) =
    let subst =
      VarSet.fold (fun a acc -> (a, TVar (fresh ())) :: acc) vars []
    in
    apply_subst_ty subst ty
  in

  let lookup x env =
    try Some (Env.find x env) with Not_found -> None
  in

  let rec infer gamma expr =
    match expr with
    | Unit -> (TUnit, [])
    | Bool _ -> (TBool, [])
    | Int _ -> (TInt, [])
    | Float _ -> (TFloat, [])
    | Nil ->
        let a = TVar (fresh ()) in
        (TList a, [])
    | ENone ->
        let a = TVar (fresh ()) in
        (TOption a, [])

    | Var x ->
        (match lookup x gamma with
         | Some scheme ->
             let t = instantiate scheme in
             (t, [])
         | None -> raise Type_error)

    | Assert e1 ->
        (match e1 with
         | Bool false ->
             let a = TVar (fresh ()) in
             (a, [])
         | _ ->
             let (t1, c1) = infer gamma e1 in
             (TUnit, (t1, TBool) :: c1))


    | ESome e1 ->
        let (t1, c1) = infer gamma e1 in
        (TOption t1, c1)

    | Bop (op, e1, e2) ->
        let (t1, c1) = infer gamma e1 in
        let (t2, c2) = infer gamma e2 in
        (match op with
         | Add | Sub | Mul | Div | Mod ->
             (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
         | AddF | SubF | MulF | DivF | PowF ->
             (TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2)
         | And | Or ->
             (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
         | Lt | Lte | Gt | Gte ->
           (TBool, c1 @ c2)
         | Eq | Neq ->
           (TBool, c1 @ c2)

         | Cons ->
             let a = TVar (fresh ()) in
             (TList a, (t1, a) :: (t2, TList a) :: c1 @ c2)
         | Comma ->
             (TPair (t1, t2), c1 @ c2))

    | If (e1, e2, e3) ->
        let (t1, c1) = infer gamma e1 in
        let (t2, c2) = infer gamma e2 in
        let (t3, c3) = infer gamma e3 in
        (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)

    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        let (ts, cs) = infer gamma matched in
        let a = TVar (fresh ()) in
        let cs' = (ts, TList a) :: cs in
        let gamma_cons =
          Env.add hd_name (Forall (VarSet.empty, a))
            (Env.add tl_name (Forall (VarSet.empty, TList a)) gamma)
        in
        let (t_cons, c_cons) = infer gamma_cons cons_case in
        let (t_nil, c_nil) = infer gamma nil_case in
        (t_cons, cs' @ c_cons @ (t_cons, t_nil) :: c_nil)

    | OptMatch { matched; some_name; some_case; none_case } ->
        let (ts, cs) = infer gamma matched in
        let a = TVar (fresh ()) in
        let cs' = (ts, TOption a) :: cs in
        let gamma_some =
          Env.add some_name (Forall (VarSet.empty, a)) gamma
        in
        let (t_some, c_some) = infer gamma_some some_case in
        let (t_none, c_none) = infer gamma none_case in
        (t_some, cs' @ c_some @ c_none @ [(t_some, t_none)])


    | PairMatch { matched; fst_name; snd_name; case } ->
    let (ts, cs) = infer gamma matched in
    let a = TVar (fresh ()) in
    let b = TVar (fresh ()) in
    let cs' = (ts, TPair(a, b)) :: cs in
    let gamma' =
      Env.add fst_name (Forall (VarSet.empty, a))
        (Env.add snd_name (Forall (VarSet.empty, b)) gamma)
    in
    let (t_case, c_case) = infer gamma' case in
    (t_case, c_case @ cs')


    | Fun (arg, ty_opt, body) ->
        (match ty_opt with
         | Some ty_arg ->
             let gamma' =
               Env.add arg (Forall (VarSet.empty, ty_arg)) gamma
             in
             let (t_body, c_body) = infer gamma' body in
             (TFun (ty_arg, t_body), c_body)
         | None ->
             let a = TVar (fresh ()) in
             let gamma' =
               Env.add arg (Forall (VarSet.empty, a)) gamma
             in
             let (t_body, c_body) = infer gamma' body in
             (TFun (a, t_body), c_body))

    | App (e1, e2) ->
        let (t1, c1) = infer gamma e1 in
        let (t2, c2) = infer gamma e2 in
        let a = TVar (fresh ()) in
        (a, (t1, TFun (t2, a)) :: c1 @ c2)

    | Annot (e1, ty) ->
        let (t1, c1) = infer gamma e1 in
        (t1, (t1, ty) :: c1)

    | Let { is_rec = false; name; binding; body } ->
        let (tb, cb) = infer gamma binding in
        (match principle_type tb cb with
         | None -> raise Type_error
         | Some scheme ->
             let gamma' = Env.add name scheme gamma in
             infer gamma' body)

| Let { is_rec = true; name; binding; body } ->
    (match binding with
     | Fun _ ->
         let a = TVar (fresh ()) in
         let gamma_tmp = Env.add name (Forall (VarSet.empty, a)) gamma in
         let (tb, cb) = infer gamma_tmp binding in
         let cs = (tb, a) :: cb in
         (match unify cs [] with
          | None -> raise Type_error
          | Some s ->
              let tb' = apply_subst_ty s tb in
              let vars = free_ty_vars tb' in
              let scheme = Forall (vars, tb') in
              let gamma' = Env.add name scheme gamma in
              infer gamma' body)
     | _ -> raise Type_error)

  in

  try
    let (t, cs) = infer ctxt e in
    principle_type t cs
  with
  | Type_error -> None

let is_well_typed (p : prog) : bool =
  let rec nest = function
    | [] -> Unit
    | [{ is_rec; name; binding }] ->
        Let { is_rec; name; binding; body = Var name }
    | { is_rec; name; binding } :: rest ->
        Let { is_rec; name; binding; body = nest rest }
  in
  match type_of Env.empty (nest p) with
  | Some _ -> true
  | None -> false


exception AssertFail
exception DivByZero
exception CompareFunVals

let rec eval_expr (env : dyn_env) (e : expr) : value = 
  match e with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | Float f -> VFloat f
  | Nil -> VList []
  | ENone -> VNone

  | Var x ->
      Env.find x env

  | Assert e1 ->
      (match eval_expr env e1 with
       | VBool true -> VUnit
       | VBool false -> raise AssertFail
       | _ -> raise AssertFail)

  | ESome e1 ->
      let v1 = eval_expr env e1 in
      VSome v1

  | Bop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
       | Add, VInt a, VInt b -> VInt (a + b)
       | Sub, VInt a, VInt b -> VInt (a - b)
       | Mul, VInt a, VInt b -> VInt (a * b)
       | Div, VInt a, VInt b ->
            if b = 0 then raise DivByZero else VInt (a / b)
       | Mod, VInt a, VInt b ->
            if b = 0 then raise DivByZero else VInt (a mod b)

       | AddF, VFloat a, VFloat b -> VFloat (a +. b)
       | SubF, VFloat a, VFloat b -> VFloat (a -. b)
       | MulF, VFloat a, VFloat b -> VFloat (a *. b)
       | DivF, VFloat a, VFloat b -> VFloat (a /. b)
       | PowF, VFloat a, VFloat b -> VFloat (a ** b)

       | And, VBool a, VBool b -> VBool (a && b)
       | Or, VBool a, VBool b -> VBool (a || b)

       | Lt, _, _
       | Lte, _, _
       | Gt, _, _
       | Gte, _, _
       | Eq, _, _
       | Neq, _, _ ->
            (match v1, v2 with
             | VClos _, _ | _, VClos _ -> raise CompareFunVals
             | _ ->
                 let b =
                   match op with
                   | Lt -> v1 < v2
                   | Lte -> v1 <= v2
                   | Gt -> v1 > v2
                   | Gte -> v1 >= v2
                   | Eq -> v1 = v2
                   | Neq -> v1 <> v2
                   | _ -> false
                 in
                 VBool b)

       | Cons, vhd, VList tl -> VList (vhd :: tl)
       | Comma, v1, v2 -> VPair (v1, v2)

       | _ -> failwith "invalid bop operands")

  | If (e1, e2, e3) ->
      (match eval_expr env e1 with
       | VBool true -> eval_expr env e2
       | VBool false -> eval_expr env e3
       | _ -> failwith "if expects bool")

  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match eval_expr env matched with
       | VList (h :: t) ->
            let env' =
              Env.add hd_name h (Env.add tl_name (VList t) env)
            in
            eval_expr env' cons_case
       | VList [] ->
            eval_expr env nil_case
       | _ -> failwith "list match expects list")

  | OptMatch { matched; some_name; some_case; none_case } ->
      (match eval_expr env matched with
       | VSome v ->
            let env' = Env.add some_name v env in
            eval_expr env' some_case
       | VNone ->
            eval_expr env none_case
       | _ -> failwith "option match expects option")

  | PairMatch { matched; fst_name; snd_name; case } ->
      (match eval_expr env matched with
       | VPair (v1, v2) ->
            let env' =
              Env.add fst_name v1 (Env.add snd_name v2 env)
            in
            eval_expr env' case
       | _ -> failwith "pair match expects pair")

  | Fun (arg, _, body) ->
      VClos { name = None; arg; body; env }

  | App (e1, e2) ->
      let vf = eval_expr env e1 in
      let va = eval_expr env e2 in
      (match vf with
       | VClos { name; arg; body; env = clos_env } ->
            let env_self =
              match name with
              | None -> clos_env
              | Some f -> Env.add f vf clos_env
            in
            let env' = Env.add arg va env_self in
            eval_expr env' body
       | _ -> failwith "apply non-function")

  | Annot (e1, _) ->
      eval_expr env e1

  | Let { is_rec = false; name; binding; body } ->
      let v1 = eval_expr env binding in
      let env' = Env.add name v1 env in
      eval_expr env' body

  | Let { is_rec = true; name; binding; body } ->
      let v1 = eval_expr env binding in
      (match v1 with
       | VClos { name = _; arg; body = fun_body; env = clos_env } ->
            let rec_clos =
              VClos { name = Some name; arg; body = fun_body; env = clos_env }
            in
            let env' = Env.add name rec_clos env in
            eval_expr env' body
       | _ -> failwith "let rec expects function")

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError