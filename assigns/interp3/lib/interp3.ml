include Utils


let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None


type subst = (ident * ty) list

exception UnifyError of ty * ty

let rec apply_subst_ty (s : subst) (t : ty) : ty =
  match t with
  | TUnit | TInt | TFloat | TBool -> t
  | TVar a ->
      (match List.assoc_opt a s with
       | Some t' -> t'
       | None -> t)
  | TList t1      -> TList   (apply_subst_ty s t1)
  | TOption t1    -> TOption (apply_subst_ty s t1)
  | TPair (t1,t2) -> TPair   (apply_subst_ty s t1, apply_subst_ty s t2)
  | TFun  (t1,t2) -> TFun    (apply_subst_ty s t1, apply_subst_ty s t2)

let apply_subst_constr (s : subst) ((t1,t2) : constr) : constr =
  (apply_subst_ty s t1, apply_subst_ty s t2)

let apply_subst_constrs (s : subst) (cs : constr list) : constr list =
  List.map (apply_subst_constr s) cs

let rec occurs (a : ident) (t : ty) : bool =
  match t with
  | TUnit | TInt | TFloat | TBool -> false
  | TVar b -> a = b
  | TList t1
  | TOption t1 -> occurs a t1
  | TPair (t1,t2)
  | TFun (t1,t2) -> occurs a t1 || occurs a t2

let rec unify (cs : constr list) : subst =
  match cs with
  | [] -> []
  | (t1,t2) :: rest ->
      if t1 = t2 then
        unify rest
      else
        match t1, t2 with
        | TVar a, t
        | t, TVar a ->
            if occurs a t then
              raise (UnifyError (t1,t2))
            else
              let s = [ (a, t) ] in
              let rest' = apply_subst_constrs s rest in
              let s_rest = unify rest' in
              (* composition: apply later substitutions first *)
              s_rest @ s

        | TFun (a1,b1), TFun (a2,b2) ->
            unify ((a1,a2) :: (b1,b2) :: rest)

        | TList t1, TList t2
        | TOption t1, TOption t2 ->
            unify ((t1,t2) :: rest)

        | TPair (x1,y1), TPair (x2,y2) ->
            unify ((x1,x2) :: (y1,y2) :: rest)

        | (TUnit, TUnit)
        | (TInt, TInt)
        | (TFloat, TFloat)
        | (TBool, TBool) ->
            unify rest

        | _ ->
            raise (UnifyError (t1,t2))

(* ---------- Free type variables and generalization ---------- *)

let rec free_ty_vars (t : ty) : VarSet.t =
  match t with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar a -> VarSet.singleton a
  | TList t1
  | TOption t1 -> free_ty_vars t1
  | TPair (t1,t2)
  | TFun (t1,t2) ->
      VarSet.union (free_ty_vars t1) (free_ty_vars t2)

let generalize (t : ty) : ty_scheme =
  let vars = free_ty_vars t in
  Forall (vars, t)

(* ---------- principle_type ---------- *)

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  try
    let s   = unify cs in
    let ty' = apply_subst_ty s ty in
    Some (generalize ty')
  with UnifyError _ ->
    None


let type_of (_ctxt: stc_env) (_e : expr) : ty_scheme option = assert false

let is_well_typed (_p : prog) : bool = assert false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (_env : dyn_env) (_e : expr) : value = assert false

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in
  eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
      if is_well_typed prog
      then Ok (eval prog)
      else Error TypeError
  | None -> Error ParseError
