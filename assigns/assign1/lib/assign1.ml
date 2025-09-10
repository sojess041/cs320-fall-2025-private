
let num_factors (n : int) : int = 
  let rec loop n d count =
    if d * d > n then
      if n > 1 then count + 1 else count
    else if n mod d = 0 then
      loop (n/d) d (count + 1)

    else 
      loop n (d+1) count
  in
  loop n 2 0



let perfect_power (i : int) (n : int) : bool = 
  if i = 1 then true
    else if n = 0 then true
      else
  let res = abs n in
  let loop (base : int) (exp : int) (limit : int) : int =
    let rec build acc expo = 
      if expo = 0 then acc 
        else if acc > limit / base then limit + 1
          else build (acc * base) (expo - 1)
        in build 1 exp 
      in
      let rec test k = 
        let pow = loop k i res in 
        if pow = res then true
          else if pow > res || k > res then false
            else test (k + 1)
          in test 1




          

let collatz (n : int) : int =
  let rec coll i count =
  if i = 1 then count 
    else if i mod 2 = 0 then 
      coll (i/2) (count + 1)
      else coll (3 * i * 1) (count + 1)
    in
    coll n 0

let tst_records (i : int) : int = 
  let rec loop n seq found = 
    let a = collatz i in 
    if a > seq then 
      if found = i then n 
        else loop (n + 1) a (found + 1)
        else loop (n+1 ) seq found
      in 
      loop 1 (-1) 0 
