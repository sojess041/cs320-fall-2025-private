let rec rev_append (l : 'a list) (r : 'a list) : 'a list =
  match l with
  | [] -> r
  | x :: xs -> rev_append xs (x :: r)

let is_whitespace c = List.mem c [' '; '\n'; '\t'; '\r']

let split_on_whitespace (s : string) : string list =
  let rec go acc i j =
    if i + j >= String.length s
    then List.rev (String.sub s i j :: acc)
    else
      if is_whitespace (String.get s (i + j))
      then go (String.sub s i j :: acc) (i + j + 1) 0
      else go acc i (j + 1)
  in go [] 0 0

let sep_on_whitespace (s : string) : string list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | h :: t ->
      if h = ""
      then go acc t
      else go (h :: acc) t
  in go [] (split_on_whitespace s)

type registers = (int * int) list

let load (l : int list) : registers =
  let rec aux i xs =
    match xs with
    | [] -> []
    | h :: t ->
        if h = 0 then aux (i + 1) t          
        else (i, h) :: aux (i + 1) t
  in
  aux 0 l

let rec lookup (rs : registers) (i : int) : int = 
  match rs with 
  |[] -> 0
  |(k, j) :: t->
    if k = i then j
    else 
      lookup t i


let rec incr (rs : registers) (i : int) : registers = 
  match rs with 
  |[] -> [(i, 1)]         
  | (a, b) :: t->
    if a = i then (a, b + 1) :: t
    else if a >i then (i, 1) :: rs 
    else(a,b) :: incr t i 



let rec zero (rs : registers) (i : int) : registers =
  match rs with 
  |[] -> []
  |(c, d) :: t ->
    if c = i then t    
    else (c, d) :: zero t i      



let rec update (rs : registers) (i : int) (v : int) : registers =
  match rs with
  | [] -> if v = 0 then [] else [(i, v)]
  | (j, w) :: t ->
      if j = i then
        if v = 0 then t else (i, v) :: t
      else if j > i then
        if v = 0 then rs else (i, v) :: rs
      else
        (j, w) :: update t i v
let transfer (rs : registers) (i : int) (j : int) : registers =
  let v = lookup rs i in
  update rs j v

  let parse_urm (tokens : string list) : int list list =
    let rec aux toks =
      match toks with
      | [] -> []
      | "Z" :: i :: rest -> [0; int_of_string i] :: aux rest
      | "I" :: i :: rest -> [1; int_of_string i] :: aux rest
      | "T" :: i :: j :: rest -> [2; int_of_string i; int_of_string j] :: aux rest
      | "J" :: i :: j :: k :: rest -> [3; int_of_string i; int_of_string j; int_of_string k] :: aux rest
      | _ -> failwith "invalid program"
    in
    aux tokens

let eval_urm (prog : int list list) (rs : registers) : registers =
  let rec step rs pc =
    if pc < 0 || pc >= List.length prog then rs
    else
      match List.nth prog pc with
      | [0; i] -> step (zero rs i) (pc + 1)
      | [1; i] -> step (incr rs i) (pc + 1)
      | [2; i; j] -> step (transfer rs i j) (pc + 1)
      | [3; i; j; k] ->
          if lookup rs i = lookup rs j then step rs k
          else step rs (pc + 1)
      | _ -> failwith "invalid instruction"
  in
  step rs 0

let interp_urm (prog : string) (args : int list) : int =
  prog
  |> sep_on_whitespace
  |> parse_urm
  |> fun prog -> eval_urm prog (load args)
  |> fun rs -> lookup rs 0

(* Challenge problem: make this work for negative inputs *)
let max_urm (i : int) (j : int) : int =
  interp_urm
    "
    T 0 2
    Z 0
    J 1 3 100
    I 0
    I 3
    J 0 0 2
    "
    [i; j]

let fibonacci_urm (i : int) : int =
  interp_urm
    "
    I 2
    J 0 5 11
      T 2 3
      J 1 4 7
        I 2 I 4
        J 0 0 3
      T 3 1
      Z 4 I 5
      J 0 0 1
    T 1 0
    "
    [i]
