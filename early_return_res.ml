(* build a schema map find solution 
When a matching target is found and returned, no subsequent calculations are performed. .
 *)
let rec do_until f array predicate=
  match array with
  | [] -> []
  | hd::tl -> let hd_solution = f hd in
  match predicate hd_solution with
  | false -> do_until f tl  predicate
  | true -> hd_solution

(* ---- example ----*)
let is_l3 solution = 
  List.length solution == 3

let ntimes x n = 
  let rec iter aucc n x = 
    match n with 
    | 0 -> aucc
    | _ -> iter (x::aucc) (n-1) x
  in 
  iter [] n x;;
let n1times = ntimes 1;;
let res = 
  do_until n1times [4;1;5;3;2;6;4] is_l3;;
