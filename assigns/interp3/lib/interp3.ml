include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  try
    let subst = unify cs in
    let principle_ty = apply_subst subst ty in
    Some (generalize Env.empty principle_ty)
  with UnifyError _ -> None

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
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
