
let curry3 (f : ('a * 'b * 'c) -> 'd) : 'a -> 'b -> 'c -> 'd =
  fun a -> fun b -> fun c -> f (a, b, c)


let uncurry3 (f : 'a -> 'b -> 'c -> 'd) : ('a * 'b * 'c) -> 'd =
  fun (a, b, c) -> f a b c

  


  let filter_map (p : 'a -> 'b option) (xs : 'a list) : 'b list =
    let rec go acc = function
      | [] -> List.rev acc
      | x :: tl ->
          match p x with
          | None -> go acc tl
          | Some y -> go (y :: acc) tl
    in
    go [] xs

type 'a rtree = Node of 'a * 'a rtree list

let rec map_rtree (f : 'a -> 'b) (t : 'a rtree) : 'b rtree =
  match t with
  | Node (v, cs) -> Node (f v, List.map (map_rtree f) cs)



  let rec filter_rtree (f : 'a -> bool) (t : 'a rtree) : 'a rtree option =
    match t with
    | Node (v, cs) ->
        if not (f v) then None
        else
          let rec filter_map g = function
            | [] -> []
            | x :: xs ->
                match g x with
                | None -> filter_map g xs
                | Some y -> y :: filter_map g xs
          in
          let kept = filter_map (filter_rtree f) cs in
          Some (Node (v, kept))
type op = Add | Sub | Mul | Div

type expr =
  | Int of int
  | Var of string
  | Call of string * expr list
  | Bop of op * expr * expr

type stmt =
  | FunDef of string * string list * stmt list
  | Assign of string * expr
  | Print of expr
  | Return of expr

type prog = stmt list

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let rec string_of_expr (e : expr) : string =
match e with
| Int n -> string_of_int n
| Var x -> x
| Call (fname, args) ->
    let args_s = String.concat ", " (List.map string_of_expr args) in
    fname ^ "(" ^ args_s ^ ")"
| Bop (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"


let rec repeat (s : string) (n : int) : string =
  if n <= 0 then "" else s ^ repeat s (n - 1)
let indent (lvl : int) : string =
  repeat " " (4 * lvl)

let rec string_of_stmt_with lvl (s : stmt) : string =
  let ind = indent lvl in
  match s with
  | Assign (x, e) -> ind ^ x ^ " = " ^ string_of_expr e
  | Print e -> ind ^ "print(" ^ string_of_expr e ^ ")"
  | Return e -> ind ^ "return " ^ string_of_expr e
  | FunDef (name, params, body) ->
      let header =
        ind ^ "def " ^ name ^ "(" ^ String.concat ", " params ^ "):"
      in
      let body_lines =
        match body with
        | [] -> [ indent (lvl + 1) ^ "pass" ]  
        | _ -> List.map (string_of_stmt_with (lvl + 1)) body
      in
      String.concat "\n" (header :: body_lines)

let string_of_stmt (s : stmt) : string =
  string_of_stmt_with 0 s

let string_of_prog (p : prog) : string =
  String.concat "\n" (List.map string_of_stmt p)

