include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

(* Type Inference Helper Functions *)

(* Generate a fresh type variable identifier (string) *)
let fresh_var_counter = ref 0
let fresh () : ident =
  let n = !fresh_var_counter in
  incr fresh_var_counter;
  "\\'t" ^ string_of_int n (* Corrected escape sequence *)

(* Define substitution type alias for clarity *)
type substitution = (ident * ty) list

(* Apply substitution to a type *)
let rec apply_subst (subst : substitution) (ty : ty) : ty =
  match ty with
  | TUnit -> TUnit
  | TInt -> TInt
  | TBool -> TBool
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TVar id ->
    (match List.assoc_opt id subst with
     | Some t -> t
     | None -> TVar id)
  | TFloat -> TFloat
  | TList t -> TList (apply_subst subst t)
  | TOption t -> TOption (apply_subst subst t)
  | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)

(* Apply substitution to a constraint list *)
let apply_subst_constr (subst : substitution) (cs : constr list) : constr list =
  List.map (fun (t1, t2) -> (apply_subst subst t1, apply_subst subst t2)) cs

(* Apply substitution to a type environment and type scheme (mutually recursive) *)
let rec apply_subst_env (subst : substitution) (env : stc_env) : stc_env =
  Env.map (fun ty_scheme -> apply_subst_scheme subst ty_scheme) env

and apply_subst_scheme (subst : substitution) (ts : ty_scheme) : ty_scheme =
  match ts with
  | Forall (vars, ty) ->
    let subst_prime = List.filter (fun (id, _) -> not (VarSet.mem id vars)) subst in
    Forall (vars, apply_subst subst_prime ty)

(* Compose substitutions *)
let compose_subst (s2 : substitution) (s1 : substitution) : substitution =
  let s1_applied = List.map (fun (id, t) -> (id, apply_subst s2 t)) s1 in
  List.fold_left (fun acc (id, t) ->
    if List.mem_assoc id s1 then acc else (id, t) :: acc
  ) s1_applied s2

(* Free type variables in a type *)
let rec ftv (ty : ty) : VarSet.t =
  match ty with
  | TUnit | TInt | TBool | TFloat -> VarSet.empty
  | TFun (t1, t2) -> VarSet.union (ftv t1) (ftv t2)
  | TVar id -> VarSet.singleton id
  | TList t -> ftv t
  | TOption t -> ftv t
  | TPair (t1, t2) -> VarSet.union (ftv t1) (ftv t2)

(* Free type variables in a type environment and type scheme (mutually recursive) *)
let rec ftv_env (env : stc_env) : VarSet.t =
  Env.fold (fun _ ts acc -> VarSet.union (ftv_scheme ts) acc) env VarSet.empty

and ftv_scheme (ts : ty_scheme) : VarSet.t =
  match ts with
  | Forall (vars, ty) -> VarSet.diff (ftv ty) vars

(* Unification Algorithm *)
exception UnifyError of ty * ty

let rec unify (cs : constr list) : substitution =
  match cs with
  | [] -> []
  | (t1, t2) :: rest ->
    if t1 = t2 then unify rest
    else
      match (t1, t2) with
      | (TFun (a, b), TFun (c, d)) -> unify ((a, c) :: (b, d) :: rest)
      | (TList a, TList b) -> unify ((a, b) :: rest)
      | (TOption a, TOption b) -> unify ((a, b) :: rest)
      | (TPair (a, b), TPair (c, d)) -> unify ((a, c) :: (b, d) :: rest)
      | (TVar id, t) | (t, TVar id) ->
        if VarSet.mem id (ftv t)
        then raise (UnifyError (t1, t2))
        else
          let subst = [(id, t)] in
          compose_subst (unify (apply_subst_constr subst rest)) subst
      | _ -> raise (UnifyError (t1, t2))

(* Generalize a type into a type scheme *)
let generalize (env : stc_env) (ty : ty) : ty_scheme =
  let env_ftv = ftv_env env in
  let ty_ftv = ftv ty in
  let vars = VarSet.diff ty_ftv env_ftv in
  Forall (vars, ty)

(* Instantiate a type scheme *)
let instantiate (ts : ty_scheme) : ty =
  match ts with
  | Forall (vars, ty) ->
    let subst = VarSet.fold (fun id acc -> (id, TVar (fresh ())) :: acc) vars [] in
    apply_subst subst ty

(* Type Inference Algorithm (W) *)
let rec infer (env : stc_env) (e : expr) : ty * constr list =
  match e with
  | Unit -> (TUnit, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Bool _ -> (TBool, [])
  | Nil -> (TList (TVar (fresh ())), [])
  | ENone -> (TOption (TVar (fresh ())), [])
  | Var x ->
    (match Env.find_opt x env with
     | Some ts -> (instantiate ts, [])
     | None -> failwith ("Unbound variable: " ^ x))
  | Fun (x, _, body) ->
    let tv_id = fresh () in
    let tv = TVar tv_id in
    let env' = Env.add x (Forall (VarSet.empty, tv)) env in (* Corrected env' *)
    let (tbody, cbody) = infer env' body in
    (TFun (tv, tbody), cbody)
  | App (e1, e2) ->
    let (t1, c1) = infer env e1 in
    let (t2, c2) = infer env e2 in
    let tv_id = fresh () in
    let tv = TVar tv_id in
    (tv, (t1, TFun (t2, tv)) :: c1 @ c2)
  | Let {is_rec; name; binding; body} ->
    if is_rec then
      let tv_binding_id = fresh () in
      let tv_binding = TVar tv_binding_id in
      let env_binding = Env.add name (Forall (VarSet.empty, tv_binding)) env in
      let (t_binding, c_binding) = infer env_binding binding in
      let subst_binding = unify ((tv_binding, t_binding) :: c_binding) in
      let env' = apply_subst_env subst_binding env in (* Corrected env' *)
      let t_binding' = apply_subst subst_binding tv_binding in (* Corrected t_binding' *)
      let ts_binding = generalize env' t_binding' in
      let env_body = Env.add name ts_binding env' in
      let (t_body, c_body) = infer env_body body in
      (t_body, apply_subst_constr subst_binding c_body @ c_body)
    else
      let (t_binding, c_binding) = infer env binding in
      let subst_binding = unify c_binding in
      let env' = apply_subst_env subst_binding env in (* Corrected env' *)
      let t_binding' = apply_subst subst_binding t_binding in (* Corrected t_binding' *)
      let ts_binding = generalize env' t_binding' in
      let env_body = Env.add name ts_binding env' in
      let (t_body, c_body) = infer env_body body in
      (t_body, apply_subst_constr subst_binding c_body @ c_body)
  | Bop (op, e1, e2) ->
      let (t1, c1) = infer env e1 in
      let (t2, c2) = infer env e2 in
      let constraints = c1 @ c2 in
      (match op with
       | Add | Sub | Mul | Div | Mod -> (TInt, (t1, TInt) :: (t2, TInt) :: constraints)
       | AddF | SubF | MulF | DivF | PowF -> (TFloat, (t1, TFloat) :: (t2, TFloat) :: constraints)
       | Lt | Gt | Lte | Gte -> 
           let tv_id = fresh() in let tv = TVar tv_id in
           (TBool, (t1, tv) :: (t2, tv) :: constraints)
       | Eq | Neq -> 
           let tv_id = fresh() in let tv = TVar tv_id in
           (TBool, (t1, tv) :: (t2, tv) :: constraints)
       | And | Or -> (TBool, (t1, TBool) :: (t2, TBool) :: constraints)
       | Comma -> (TPair (t1, t2), constraints)
       | Cons -> 
           let tv_id = fresh() in let tv = TVar tv_id in
           (TList tv, (t1, tv) :: (t2, TList tv) :: constraints)
      )
  | If (cond, thn, els) ->
      let (t_cond, c_cond) = infer env cond in
      let (t_thn, c_thn) = infer env thn in
      let (t_els, c_els) = infer env els in
      (t_thn, (t_cond, TBool) :: (t_thn, t_els) :: c_cond @ c_thn @ c_els)
  | Assert e1 -> 
      let (t1, c1) = infer env e1 in
      (TUnit, (t1, TBool) :: c1)
  | ESome e1 ->
      let (t1, c1) = infer env e1 in
      (TOption t1, c1)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let (t_matched, c_matched) = infer env matched in
      let tv_elem_id = fresh () in let tv_elem = TVar tv_elem_id in
      let tv_result_id = fresh () in let tv_result = TVar tv_result_id in
      let env_cons = Env.add hd_name (Forall (VarSet.empty, tv_elem)) (Env.add tl_name (Forall (VarSet.empty, TList tv_elem)) env) in
      let (t_cons, c_cons) = infer env_cons cons_case in
      let (t_nil, c_nil) = infer env nil_case in
      (tv_result, 
       (t_matched, TList tv_elem) :: 
       (t_cons, tv_result) :: 
       (t_nil, tv_result) :: 
       c_matched @ c_cons @ c_nil)
  | OptMatch { matched; some_name; some_case; none_case } ->
      let (t_matched, c_matched) = infer env matched in
      let tv_elem_id = fresh () in let tv_elem = TVar tv_elem_id in
      let tv_result_id = fresh () in let tv_result = TVar tv_result_id in
      let env_some = Env.add some_name (Forall (VarSet.empty, tv_elem)) env in
      let (t_some, c_some) = infer env_some some_case in
      let (t_none, c_none) = infer env none_case in
      (tv_result,
       (t_matched, TOption tv_elem) ::
       (t_some, tv_result) ::
       (t_none, tv_result) ::
       c_matched @ c_some @ c_none)
  | PairMatch { matched; fst_name; snd_name; case } ->
      let (t_matched, c_matched) = infer env matched in
      let tv_fst_id = fresh () in let tv_fst = TVar tv_fst_id in
      let tv_snd_id = fresh () in let tv_snd = TVar tv_snd_id in
      let env_case = Env.add fst_name (Forall (VarSet.empty, tv_fst)) (Env.add snd_name (Forall (VarSet.empty, tv_snd)) env) in
      let (t_case, c_case) = infer env_case case in
      (t_case,
       (t_matched, TPair (tv_fst, tv_snd)) ::
       c_matched @ c_case)
  | Annot (e1, ty_annot) ->
      let (t1, c1) = infer env e1 in
      (ty_annot, (t1, ty_annot) :: c1)

(* Principle Type Calculation *)
let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  try
    let subst = unify cs in
    let principle_ty = apply_subst subst ty in
    Some (generalize Env.empty principle_ty)
  with UnifyError _ -> None

(* Type Of Expression *)
let type_of (ctxt: stc_env) (e : expr) : ty_scheme option =
  try
    let (ty, cs) = infer ctxt e in
    principle_type ty cs
  with
  | UnifyError _ -> None
  | Failure _ -> None

(* Check if Program is Well-Typed *)
let is_well_typed (p : prog) : bool =
  let rec check_prog env prog =
    match prog with
    | [] -> true
    | {is_rec; name; binding; _} :: rest_prog ->
        if is_rec then
          let tv_binding_id = fresh () in
          let tv_binding = TVar tv_binding_id in
          let env_binding = Env.add name (Forall (VarSet.empty, tv_binding)) env in
          (match infer env_binding binding with
           | (t_binding, c_binding) ->
               (try
                  let subst_binding = unify ((tv_binding, t_binding) :: c_binding) in
                  let env' = apply_subst_env subst_binding env in (* Corrected env' *)
                  let t_binding' = apply_subst subst_binding tv_binding in (* Corrected t_binding' *)
                  let ts_binding = generalize env' t_binding' in
                  let env_next = Env.add name ts_binding env' in
                  check_prog env_next rest_prog
                with UnifyError _ -> false)
           | exception Failure _ -> false
           | exception UnifyError _ -> false)
        else
          (match type_of env binding with
           | Some binding_ts ->
               let env' = Env.add name binding_ts env in (* Corrected env' *)
               check_prog env' rest_prog
           | None -> false)
  in
  check_prog Env.empty p


exception AssertFail
exception DivByZero
exception CompareFunVals
(* Removed TypeError exception as it conflicts with the error type constructor *)

(* Evaluation *) 
let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | Int n -> VInt n
  | Float f -> VFloat f
  | Bool b -> VBool b
  | Nil -> VList []
  | ENone -> VNone
  | Var x -> 
    (match Env.find_opt x env with
     | Some v -> v
     | None -> failwith ("Unbound variable during evaluation: " ^ x))
  | Fun (x, _, body) -> VClos {name=None; arg=x; body=body; env=env}
  | App (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
     | VClos {arg; body; env=closure_env; name=rec_name} ->
       let env_arg = Env.add arg v2 closure_env in
       let env_rec = match rec_name with 
                   | Some fname -> Env.add fname v1 env_arg 
                   | None -> env_arg 
       in
       eval_expr env_rec body
     | _ -> failwith "Application of non-function value")
  | Let {is_rec; name; binding; body} ->
    if is_rec then
      (match binding with 
       | Fun (arg, _, fun_body) -> 
           let clos = VClos {name=Some name; arg=arg; body=fun_body; env=env} in
           let env_body = Env.add name clos env in
           eval_expr env_body body
       | _ -> failwith "Recursive let binding must be a function")
    else
      let v_binding = eval_expr env binding in
      let env_body = Env.add name v_binding env in
      eval_expr env_body body
  | Bop (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match (op, v1, v2) with
     | (Add, VInt n1, VInt n2) -> VInt (n1 + n2)
     | (Sub, VInt n1, VInt n2) -> VInt (n1 - n2)
     | (Mul, VInt n1, VInt n2) -> VInt (n1 * n2)
     | (Div, VInt n1, VInt n2) -> if n2 = 0 then raise DivByZero else VInt (n1 / n2)
     | (Mod, VInt n1, VInt n2) -> if n2 = 0 then raise DivByZero else VInt (n1 mod n2)
     | (AddF, VFloat f1, VFloat f2) -> VFloat (f1 +. f2)
     | (SubF, VFloat f1, VFloat f2) -> VFloat (f1 -. f2)
     | (MulF, VFloat f1, VFloat f2) -> VFloat (f1 *. f2)
     | (DivF, VFloat f1, VFloat f2) -> if f2 = 0.0 then raise DivByZero else VFloat (f1 /. f2)
     | (PowF, VFloat f1, VFloat f2) -> VFloat (f1 ** f2)
     | (Lt, VInt n1, VInt n2) -> VBool (n1 < n2)
     | (Lt, VFloat f1, VFloat f2) -> VBool (f1 < f2)
     | (Lte, VInt n1, VInt n2) -> VBool (n1 <= n2)
     | (Lte, VFloat f1, VFloat f2) -> VBool (f1 <= f2)
     | (Gt, VInt n1, VInt n2) -> VBool (n1 > n2)
     | (Gt, VFloat f1, VFloat f2) -> VBool (f1 > f2)
     | (Gte, VInt n1, VInt n2) -> VBool (n1 >= n2)
     | (Gte, VFloat f1, VFloat f2) -> VBool (f1 >= f2)
     | (Eq, VClos _, _) | (Eq, _, VClos _) -> raise CompareFunVals
     | (Eq, _, _) -> VBool (v1 = v2)
     | (Neq, VClos _, _) | (Neq, _, VClos _) -> raise CompareFunVals
     | (Neq, _, _) -> VBool (v1 <> v2)
     | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
     | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
     | (Comma, _, _) -> VPair (v1, v2)
     | (Cons, vh, VList vt) -> VList (vh :: vt)
     | (Cons, _, _) -> failwith "Cons applied to non-list tail"
     | _ -> failwith "Type error or unimplemented binary operation during evaluation")
  | If (cond, thn, els) ->
    let v_cond = eval_expr env cond in
    (match v_cond with
     | VBool true -> eval_expr env thn
     | VBool false -> eval_expr env els
     | _ -> failwith "If condition evaluated to non-boolean")
  | Assert e1 -> 
      let v1 = eval_expr env e1 in
      (match v1 with
       | VBool true -> VUnit
       | VBool false -> raise AssertFail
       | _ -> failwith "Assert condition evaluated to non-boolean")
  | ESome e1 -> VSome (eval_expr env e1)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let v_matched = eval_expr env matched in
      (match v_matched with
       | VList [] -> eval_expr env nil_case
       | VList (h :: t) -> 
           let env' = Env.add hd_name h (Env.add tl_name (VList t) env) in (* Corrected env' *)
           eval_expr env' cons_case
       | _ -> failwith "List match applied to non-list value")
  | OptMatch { matched; some_name; some_case; none_case } ->
      let v_matched = eval_expr env matched in
      (match v_matched with
       | VNone -> eval_expr env none_case
       | VSome v -> 
           let env' = Env.add some_name v env in (* Corrected env' *)
           eval_expr env' some_case
       | _ -> failwith "Option match applied to non-option value")
  | PairMatch { matched; fst_name; snd_name; case } ->
      let v_matched = eval_expr env matched in
      (match v_matched with
       | VPair (v1, v2) ->
           let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in (* Corrected env' *)
           eval_expr env' case
       | _ -> failwith "Pair match applied to non-pair value")
  | Annot (e1, _) -> eval_expr env e1

let eval p =
  let rec build_env env prog = 
    match prog with
    | [] -> env
    | {is_rec; name; binding; _} :: rest ->
        if is_rec then
          (match binding with 
           | Fun (arg, _, fun_body) -> 
               let clos = VClos {name=Some name; arg=arg; body=fun_body; env=env} in
               build_env (Env.add name clos env) rest
           | _ -> failwith "Recursive let binding must be a function")
        else
          let v_binding = eval_expr env binding in
          build_env (Env.add name v_binding env) rest
  in
  let final_env = build_env Env.empty p in
  match List.rev p with 
  | [] -> VUnit
  | {name; _} :: _ -> 
      match Env.find_opt name final_env with
      | Some v -> v
      | None -> failwith "Failed to find final value in environment"

let interp input =
  match parse input with
  | Some prog ->
    (try 
      if is_well_typed prog
      then 
        try Ok (eval prog)
        with 
        
        | DivByZero -> Error TypeError
        | CompareFunVals -> Error TypeError
        | AssertFail -> Error TypeError
        | Failure _ -> Error TypeError (* Catch other runtime failures as TypeError *)
      else Error TypeError (* Static type error *)
    with 
    (* Map inference/unification errors to TypeError (no args) *) 
    | UnifyError _ -> Error TypeError
    | Failure _ -> Error TypeError (* Catch other inference failures as TypeError *)
    )
  | None -> Error ParseError (* Parsing failed *)