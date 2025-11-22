include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec desugar_expr (e : sfexpr) : expr =
    match e with
    | SUnit -> Unit
    | SBool b -> Bool b
    | SNum n -> Num n
    | SVar x -> Var x
    | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)
    | SIf (e1, e2, e3) -> If (desugar_expr e1, desugar_expr e2, desugar_expr e3)
    | SAssert e -> Assert (desugar_expr e)
    | SApp es -> 
        begin match es with
        | [] -> failwith "Empty application not allowed"
        | [e] -> desugar_expr e
        | e::es' -> 
            List.fold_left 
              (fun acc arg -> App (acc, desugar_expr arg)) 
              (desugar_expr e) es'
        end
    | SFun {args; body} ->
        let rec curry args body =
          match args with
          | [] -> desugar_expr body
          | (arg, ty)::rest -> Fun (arg, ty, curry rest body)
        in
        curry args body
    | SLet {is_rec; name; args; ty; binding; body} ->
        let binding' =
          if args = [] then desugar_expr binding
          else
            let rec curry args binding =
              match args with
              | [] -> desugar_expr binding
              | (arg, arg_ty)::rest -> Fun (arg, arg_ty, curry rest binding)
            in
            curry args binding
        in
        let ty' =
          if args = [] then ty
          else
            List.fold_right 
              (fun (_, arg_ty) acc -> FunTy (arg_ty, acc))
              args ty
        in
        Let {is_rec; name; ty=ty'; binding=binding'; body=desugar_expr body}
  
  let desugar (p : prog) : expr =
    match p with
    | [] -> Unit  
    | toplets ->
        let rec process_toplets = function
          | [] -> Unit  
          | [{is_rec; name; args; ty; binding}] ->
              let binding' =
                if args = [] then desugar_expr binding
                else
                  let rec curry args binding =
                    match args with
                    | [] -> desugar_expr binding
                    | (arg, arg_ty)::rest -> Fun (arg, arg_ty, curry rest binding)
                  in
                  curry args binding
              in
              let ty' =
                if args = [] then ty
                else
                  List.fold_right 
                    (fun (_, arg_ty) acc -> FunTy (arg_ty, acc))
                    args ty
              in
              Let {is_rec; name; ty=ty'; binding=binding'; body=Var name}
          | {is_rec; name; args; ty; binding}::rest ->
              let binding' =
                if args = [] then desugar_expr binding
                else
                  let rec curry args binding =
                    match args with
                    | [] -> desugar_expr binding
                    | (arg, arg_ty)::rest -> Fun (arg, arg_ty, curry rest binding)
                  in
                  curry args binding
              in
              let ty' =
                if args = [] then ty
                else
                  List.fold_right 
                    (fun (_, arg_ty) acc -> FunTy (arg_ty, acc))
                    args ty
              in
              Let {is_rec; name; ty=ty'; binding=binding'; body=process_toplets rest}
        in
        process_toplets toplets
  

        let rec type_of_expr (env : (string * ty) list) (e : expr) : (ty, error) result =
          match e with
          | Unit -> Ok UnitTy
          | Bool _ -> Ok BoolTy
          | Num _ -> Ok IntTy
          | Var x ->
              (match List.assoc_opt x env with
               | Some ty -> Ok ty
               | None -> Error (UnknownVar x))
          | Fun (x, arg_ty, body) ->
              (match type_of_expr ((x, arg_ty) :: env) body with
               | Ok ret_ty -> Ok (FunTy (arg_ty, ret_ty))
               | Error e -> Error e)
          | If (cond, then_expr, else_expr) ->
              (match type_of_expr env cond with
               | Ok BoolTy ->
                   (match type_of_expr env then_expr with
                    | Ok then_ty ->
                        (match type_of_expr env else_expr with
                         | Ok else_ty when then_ty = else_ty -> Ok then_ty
                         | Ok else_ty -> Error (IfTyErr (then_ty, else_ty))
                         | Error e -> Error e)
                    | Error e -> Error e)
               | Ok cond_ty -> Error (IfCondTyErr cond_ty)
               | Error e -> Error e)
          | Bop (op, e1, e2) ->
              (match op with
               | Add | Sub | Mul | Div | Mod ->
                   (match type_of_expr env e1 with
                    | Ok IntTy ->
                        (match type_of_expr env e2 with
                         | Ok IntTy -> Ok IntTy
                         | Ok ty -> Error (OpTyErrR (op, IntTy, ty))
                         | Error e -> Error e)
                    | Ok ty -> Error (OpTyErrL (op, IntTy, ty))
                    | Error e -> Error e)
               | Lt | Lte | Gt | Gte | Eq | Neq ->
                   (match type_of_expr env e1 with
                    | Ok IntTy ->
                        (match type_of_expr env e2 with
                         | Ok IntTy -> Ok BoolTy
                         | Ok ty -> Error (OpTyErrR (op, IntTy, ty))
                         | Error e -> Error e)
                    | Ok ty -> Error (OpTyErrL (op, IntTy, ty))
                    | Error e -> Error e)
               | And | Or ->
                   (match type_of_expr env e1 with
                    | Ok BoolTy ->
                        (match type_of_expr env e2 with
                         | Ok BoolTy -> Ok BoolTy
                         | Ok ty -> Error (OpTyErrR (op, BoolTy, ty))
                         | Error e -> Error e)
                    | Ok ty -> Error (OpTyErrL (op, BoolTy, ty))
                    | Error e -> Error e))
          | App (e1, e2) ->
              (match type_of_expr env e1 with
               | Ok (FunTy (param_ty, ret_ty)) ->
                   (match type_of_expr env e2 with
                    | Ok arg_ty when arg_ty = param_ty -> Ok ret_ty
                    | Ok arg_ty -> Error (FunArgTyErr (param_ty, arg_ty))
                    | Error e -> Error e)
               | Ok ty -> Error (FunAppTyErr ty)
               | Error e -> Error e)
          | Let {is_rec; name; ty; binding; body} ->
              if is_rec then
                match binding with
                | Fun _ ->
                    let env' = (name, ty) :: env in
                    (match type_of_expr env' binding with
                     | Ok binding_ty when binding_ty = ty -> type_of_expr env' body
                     | Ok binding_ty -> Error (LetTyErr (ty, binding_ty))
                     | Error e -> Error e)
                | _ -> Error (LetRecErr name)
              else
                (match type_of_expr env binding with
                 | Ok binding_ty when binding_ty = ty -> type_of_expr ((name, ty) :: env) body
                 | Ok binding_ty -> Error (LetTyErr (ty, binding_ty))
                 | Error e -> Error e)
          | Assert e ->
              (match type_of_expr env e with
               | Ok BoolTy -> Ok UnitTy
               | Ok ty -> Error (AssertTyErr ty)
               | Error e -> Error e)
        
        let type_of (e : expr) : (ty, error) result =
          type_of_expr [] e
        
        exception AssertFail
        exception DivByZero
        
        let rec eval_expr (env : dyn_env) (e : expr) : value =
          match e with
          | Unit -> VUnit
          | Bool b -> VBool b
          | Num n -> VNum n
          | Var x -> Env.find x env
          | Fun (arg, _, body) -> VClos { env; arg; body; name = None }
          | If (cond, then_expr, else_expr) ->
              (match eval_expr env cond with
               | VBool true -> eval_expr env then_expr
               | VBool false -> eval_expr env else_expr
               | _ -> failwith "if condition must be a boolean")
          | Bop (op, e1, e2) ->
              let v1 = eval_expr env e1 in
              let v2 = eval_expr env e2 in
              (match op, v1, v2 with
               | Add, VNum n1, VNum n2 -> VNum (n1 + n2)
               | Sub, VNum n1, VNum n2 -> VNum (n1 - n2)
               | Mul, VNum n1, VNum n2 -> VNum (n1 * n2)
               | Div, VNum n1, VNum n2 ->
                   if n2 = 0 then raise DivByZero
                   else VNum (n1 / n2)
               | Mod, VNum n1, VNum n2 ->
                   if n2 = 0 then raise DivByZero
                   else VNum (n1 mod n2)
               | Lt, VNum n1, VNum n2 -> VBool (n1 < n2)
               | Lte, VNum n1, VNum n2 -> VBool (n1 <= n2)
               | Gt, VNum n1, VNum n2 -> VBool (n1 > n2)
               | Gte, VNum n1, VNum n2 -> VBool (n1 >= n2)
               | Eq, VNum n1, VNum n2 -> VBool (n1 = n2)
               | Neq, VNum n1, VNum n2 -> VBool (n1 <> n2)
               | And, VBool b1, VBool b2 -> VBool (b1 && b2)
               | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
               | _ -> failwith "type error in binary operation")
          | App (e1, e2) ->
              let v1 = eval_expr env e1 in
              let v2 = eval_expr env e2 in
              (match v1 with
               | VClos { env = closenv; name = None; arg; body } ->
                   let env' = Env.add arg v2 closenv in
                   eval_expr env' body
               | VClos { env = closenv; name = Some f; arg; body } ->
                   let env' = Env.add f v1 closenv in
                   let env'' = Env.add arg v2 env' in
                   eval_expr env'' body
               | _ -> failwith "application to non-function")
          | Let {is_rec; name; ty=_; binding; body} ->
              if is_rec then
                match binding with
                | Fun (arg, _, fun_body) ->
                    let rec_closure = VClos { env; name = Some name; arg; body = fun_body } in
                    let env' = Env.add name rec_closure env in
                    eval_expr env' body
                | _ -> failwith "recursive binding must be a function"
              else
                let binding_val = eval_expr env binding in
                let env' = Env.add name binding_val env in
                eval_expr env' body
          | Assert e ->
              match eval_expr env e with
              | VBool true -> VUnit
              | VBool false -> raise AssertFail
              | _ -> failwith "assert condition must be a boolean"
        
        let eval (e : expr) : value =
          eval_expr Env.empty e
        
        let interp (s : string) : (value, error) result =
          match parse s with
          | None -> Error ParseErr
          | Some prog ->
              let desugared = desugar prog in
              match type_of desugared with
              | Error err -> Error err
              | Ok _ ->
                  try Ok (eval desugared)
                  with
                  | DivByZero -> Error (OpTyErrL (Div, IntTy, IntTy)) 
                  | AssertFail -> Error (AssertTyErr BoolTy)
        