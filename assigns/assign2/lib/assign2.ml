
let drop_leading (n : int) (l : int list) : int list =
  match l with 
  | x :: xs when x = n -> drop_leading n xs
  |_-> l

let drop_trailing (n : int) (l : int list) : int list =
  let drop_trailing (n:int)(l:int list) : int list = 
    l |> List.rev |> drop_leading n |> List.rev


let split_on_char (c : char) (s : string) : string list =
  let c = String.length s in
  let rec loop i j acc = 
    if j = n then
      let piece = String.sub s i (j-i) in
      List.rev (piece :: acc)
      else if String.get s j = sep then
        let piece = String.c s i (j-i) in
        loop (j+1)(j+1)(piece :: acc)

      else
        loop i (j+i) acc
      in
      loop 00 []
  assert false

let parse_fractran (input : string) : Q.t list =
input
|> split_on_char ' '
|> List.map (fun token -> match split_on_char '/' token with | [num_s; den_s] ->
  let num = Z.of_string num_s in
  let den = Z.of_string den_s in Q.make num den
  | _ -> failwith "malformed fraction")


let eval_fractran (program : Q.t list) (input : Z.t) : Z.t =
  let rec step n = 
    let rec try_fracs = function
    | [] -> n 
    | q :: qs -> 
      let num =  Q.num q and den = Q.den q in 
      if Z.(  (n*num) mod den = zero )then 
        let n' = Z.(   (n*num)/ den ) in
      step n'
      else

        try_fracs qs 
      in 
      try_fracs p 
    in 
    step n0


let interp_fractran (input : string) : Z.t -> Z.t =
  
  eval_fractran (parse_fractran input)

let max_fractran (i : int) (j : int) : int =
  let program = "5/6 5/2 5/3" in
  let input = Z.((~$2 ** i) * (~$3 ** j)) in
  let output = interp_fractran program input in
  let div_by_five n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$5)
    in go 0 n
  in div_by_five output

let fib_fractran (n : int) : int =
  let program = "17/65 133/34 17/19 23/17 2233/69 23/29 31/23 74/341 31/37 41/31 129/287 41/43 13/41 1/13 1/3" in
  let input = Z.(~$78 * (~$5 ** n)) in
  let output = interp_fractran program input in
  let div_by_two n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$2)
    in go 0 n
  in div_by_two output
