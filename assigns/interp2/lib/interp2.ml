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
  

let type_of (_ : expr) : (ty, error) result = assert false

exception AssertFail
exception DivByZero

let eval (_ : expr) :  value = assert false

let interp (_ : string) : (value, error) result = assert false
