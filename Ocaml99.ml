open Stdio
let rec print_n_list = function 
  [] -> ()
  | e::l -> printf "%N" e ; print_string " " ; print_n_list l
let rec print_s_list = function 
  [] -> ()
  | e::l -> printf "%S" e ; print_string " " ; print_s_list l

let rec print_c_list = function
  [] ->()
  | e::l -> printf "%c" e ; print_string " " ; print_c_list l
(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)*)

let rec last = function
 | [] -> None
 | [x] -> Some x
 | _ :: t-> last t

let test_last = (compare (last [1;2;3]) (Some 3)) == 0
let () = printf "%B\n" (test_last)

 let rec last_two = function
 | [] -> None
 | [x;y] -> Some (x,y)
 | _ :: t -> last_two t

let test_last_two = (compare (last_two [1;2;3]) (Some (2,3))) == 0
let () = printf "%B\n" (test_last_two)

(* let rec at k  = function
| [] -> None
| hd::tl -> if k == 1 then Some hd else at (k-1) tl *)

let rec at k l = 
  match k,l with
  | _,[]-> None
  | 1,hd::_ -> Some hd
  | _,_::tl -> at (k - 1) tl

let test_at = (compare (at 1 [1;2;3]) (Some 1)) == 0
let () = printf "%B\n" (test_at) 

let rec length = function
| [] -> 0
| _::tl -> 1 + (length tl)

let test_length = (compare (length [1;2;3]) 3) == 0
let () = printf "%B\n" (test_length)

let rec rev = function
| [] -> []
| hd::tl -> (rev tl)@[hd]
let test_rev = (compare (rev [1;2;3]) [3;2;1]) == 0
let () = printf "%B\n" (test_rev)

let rev_two l= 
  let rec iter acc l=
  match l with
  | [] -> acc
  | hd::tl -> iter (hd::acc) tl in 
  iter [] l
let test_rev_two = (compare (rev [1;2;3]) [3;2;1]) == 0
let () = printf "%B\n" (test_rev_two)
let is_palindrome x = 
  (compare x (rev x)) == 0
let test_is_palindrome = 
  (is_palindrome ["a";"b";"b";"a"]) == true

let ()  = printf "%B\n" (test_is_palindrome);;

(* There is no nested list type in OCaml, so we need to define one
  first. A node of a nested list is either an element, or a list of
  nodes. *)

type 'a node = One of 'a | Many of 'a node list

let flatten l = 
  let rec aux acc t = 
    match t with
    | [] -> acc
    | One x :: t -> aux (x::acc) t
    | Many l:: t -> aux (aux  acc l) t in 
    rev (aux [] l)
let test_flatten = compare (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]])  (["a"; "b"; "c"; "d"; "e"]) == 0
let () = printf "%B\n" (test_flatten)

let flatten_two l = 
  let rec aux acc t  = 
  match t with
  | [] -> acc
  | One x::t -> aux (acc@[x]) t
  | Many l::t -> aux (aux  acc l) t in
  aux [] l

let test_flatten_two = compare (flatten_two [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]])  (["a"; "b"; "c"; "d"; "e"]) == 0
let () = printf "%B\n" (test_flatten_two)

let compress lst= 
let rec aux acc prev lst = 
  match prev,lst with
  | _,[] -> acc
  | p,hd::tl -> if p == hd then aux acc p tl else aux (acc@[hd]) hd tl  in
  aux [] "" lst

let test_compress = 
  (compare (compress ["a";"a";"b";"b";"b";"c";"d";"e";"e"]) ["a";"b";"c";"d";"e"]) == 0

let () = printf "%B\n" (test_compress)

let pack lst = 
  let rec aux acc prev buffer lst = 
    (* [["a"]],"a" [] ["a";"a";"b"] *)
  match prev,buffer,lst with 
  | _,b,[] -> acc@[b]
  | p,b,hd::tl -> if (compare p hd)==0
    then aux acc hd (b@[hd]) tl 
    else aux (acc@[b]) hd [hd] tl in
  aux [] (List.hd lst) [] lst 

let test_pack = 
  let result = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
  let expect = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]] in
  (compare result expect) == 0

let () = printf "%B\n" (test_pack)

let encode lst = 
  let rec aux acc prev count lst = 
  match prev,count,lst with 
  | _,(n,x),[] -> acc@[(n,x)]
  | p,(n,x),hd::tl -> if (compare p hd)==0
    then aux acc hd ((n+1),hd) tl 
    else aux (acc@[(n,x)]) hd (1,hd) tl in
  aux [] (List.hd lst) (0,List.hd lst) lst

let test_encode = 
  let result = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
  let expect = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (2, "d"); (4, "e")]in
  (compare result expect) == 0

let () = printf "%B\n" (test_encode)

let encode_two lst = 
  List.map (fun l -> ((length l),(List.hd l))) (pack lst)
let test_encode_two = 
  let result = encode_two ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
  let expect = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (2, "d"); (4, "e")]in
  (compare result expect) == 0

let () = printf "%B\n" (test_encode_two)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
let encode_3th lst= 
  let foo l = 
  match (List.length l),List.hd l with
  | 1,x -> One x 
  | n,x -> Many (n,x) 
   in 
  List.map foo (pack lst)

let test_encode_3th =
  let result  = encode_3th ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
  let expect = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] in
  (compare result expect) == 0

let () = printf "%B\n" (test_encode_3th)

let expend n x = 
  let rec aux acc n x = 
    match n with
    | 0 ->  acc
    | n ->  aux (x::acc) (n-1) x in
  aux [] n x

let test_expend = 
  let result = expend 4 "a" in
  let expect = ["a";"a";"a";"a"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_expend)


let decode r =
  let rec aux acc = function
  | [] -> acc
  | One x :: tl -> aux (acc@[x]) tl
  | Many (n, x) :: tl -> aux (acc@ (expend n x)) tl in
  aux [] r

let test_decode = 
  let result = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] in
  let expect = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]  in
  (compare result expect) == 0

let () = printf "%B\n" (test_decode)

let duplicate lst = 
  let rec aux acc lst=  
  match lst with
  | [] -> acc
  | hd::tl ->aux  (acc@[hd;hd]) tl in
  aux [] lst

let test_duplicate =
  let result = duplicate ["a"; "b"; "c"; "c"; "d"] in
  let expect = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_duplicate)

let replicate lst n =
  let rec aux acc lst  = 
    match lst with 
    | [] -> acc
    | hd::tl ->aux (acc@(expend n hd)) tl  in 
  aux [] lst 

let test_replicate =
  let result = replicate ["a"; "b"; "c"] 3 in
  let expectt = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] in
  (compare result expectt) == 0

let () = printf "%B\n" (test_replicate)

let drop lst n= 
  let rec aux acc n lst= 
  match n,lst with
  | _,[] -> acc 
  | 1,_::tl -> aux acc (n-1) tl 
  | _,hd::tl -> aux (hd::acc) (n-1) tl in
  rev (aux [] n lst)

let test_drop =
  let result = drop [1;2;3;5;4] 4 in
  let expect = [1;2;3;4] in
  (compare result expect) == 0

let () = printf "%B\n" (test_drop)

let split lst n = 
  let rec aux acc lst n =
  match lst,n with 
  | [],_ -> [acc,lst]
  | _, 0->[acc,lst] 
  | hd::tl,n -> aux (acc@[hd]) tl (n-1)  in
  aux [] lst n

let test_split =
  let result = split [1;2;3;4;5] 2 in
  let expect = [[1;2],[3;4;5]] in
  (compare result expect) == 0

let () = printf "%B\n" (test_split)

let slice lst n m =
  let rec aux acc lst n m = 
    match lst,n,m with
    | [],_,_ -> acc
    | _,0,0 -> acc
    | hd::tl,0,m -> aux (acc@[hd]) tl 0 (m-1) 
    | _::tl,n,m -> aux acc tl (n-1) (m-1) in
    aux [] lst n (m+1)

let test_slice =
  let result = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 in
  let expect = ["c"; "d"; "e"; "f"; "g"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_slice)

let rotate lst n =
  match (split lst n) with
  | [(xs,ys)] -> ys @ xs
  | _ -> []

let test_rotate =
  let result = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  let expect =["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_rotate)

let test_rotate_tw0 =
  let result = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) in
  let expect = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_rotate)

let remove_at n lst = 
  drop lst (n+1)

let test_remove_at =
  let result =  remove_at 1 ["a"; "b"; "c"; "d"] in
  let expect =  ["a"; "c"; "d"] in
  (compare result expect) == 0

let () = printf "%B\n" (test_remove_at)

let insert_at n lst x =
  let rec aux acc n lst = 
    match n,lst  with
    | 0,_ -> acc@[x]@lst
    | _,hd::tl -> aux (acc@[hd]) (n-1) tl
    | _,[] -> failwith "overflow" in 
    aux [] n lst

let test_insert_at =
  let result = insert_at 1 ["a"; "b"; "c"; "d"] "z" in
  let expect = ["a"; "z"; "b"; "c"; "d"] in
  (compare result expect) == 0
let () = printf "%B\n" (test_insert_at)

let range n m = 
  let rec aux acc x y = 
    match compare x y with
    | 0 -> acc@[x]
    | -1 -> aux (acc@[x]) (x+1) y
    | 1 -> aux (acc@[x]) (x-1) y 
    | _ -> failwith "Failwith compare" in
    aux [] n m

let test_range =
  let result = range 1 5 in
  let expect = [1;2;3;4;5] in
  (compare result expect) == 0

let () = printf "%B\n" (test_range)

let test_range_two =
  let result = range 5 1 in
  let expect = [5;4;3;2;1] in
  (compare result expect) == 0

let () = printf "%B\n" (test_range_two)

let rec at2 n l =
  match n,l with
  | 0,hd::_ -> hd
  | _,_::tl -> at2 (n-1) tl 
  | _,[]-> failwith "Overflow" 

let rand_select lst n = 
  let rec aux acc n = 
    match n with
    | 0 -> acc
    | _ -> aux (acc@[at2 (Random.int (length lst)) lst])  (n-1)  in
    aux [] n

let test_rand_select =
  let result = length(rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3) in
  let expect = 3 in
  (compare result expect) == 0


let () = print_s_list (rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);;
let () = printf "%B\n" (test_rand_select)

let lotto_select n m = 
  rand_select  (range 1 m) n

let test_lotto_select =
  let result = length(lotto_select 6 49) in
  let expect = 6 in
  (compare result expect) == 0

let () = printf "%B\n" (test_lotto_select)

(* 这里用一个fold_left操作 accumulate ~f:random_choice [] 应该更好一些 *)
let permutation lst =
  let rec aux acc lst = 
    match lst with
    | [] -> acc
    | _ ->let item = Random.int (List.length lst) in 
    aux (acc@[at2 item lst]) (remove_at item lst)  in
    aux [] lst

let test_permutation =
  let result = length (permutation [1;2;3;5;4]) in
  let expect = 5 in
  (compare result expect) == 0
let () = print_n_list (permutation [1;3;3;5;4]);;
let () = printf "%B\n" (test_permutation);;


(*
Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

OCaml equivalent of Python generators https://stackoverflow.com/a/28356215/10217249
Combination https://en.wikipedia.org/wiki/Combination
*)

let foo lst =
  (* foo [1;2;3;4] == [[1;2];[1;3];[1;4]] *)
  List.map ((fun x y->[x;y])  (List.hd lst)) (List.tl lst)

(* let bar lst = 
  (* bar [1;2;3]  == [ [[1];[2;3]]; [[2];[1;3]]; [[3];[1;2]] ] *)
  [lst]] *)
let test_foo =
  let result = foo [1;2;3;4] in
  let expect = [[1;2];[1;3];[1;4]] in
  (compare result expect) == 0

let extract lst = 
  let rec aux acc lst = 
    match lst with
    | [] -> acc
    | _::tl -> aux (acc@(foo lst)) tl in
    aux [] lst

let test_extract =
  let result = extract [1;2;3] in
  let expect = [[1;2];[1;3];[2;3]] in
  (compare result expect) == 0

let () = printf "%B\n" (test_extract)

(*  what `|>` means? https://stackoverflow.com/questions/30493644/ocaml-operator *)

(* 
let extract k l = 
  let rec extr i = function
   | [] -> [] 
   | _::_ when i = 0 -> []
   (* i=1 [1;2;3]->[[1];[2];[3]] *)
   | _::_ as xs when i = 1 -> List.map (fun x -> [x]) xs
   (* | hd::tl -> List.map (fun x -> hd::x) (extr (i-1) tl) |> List.append (extr k tl) *)
   | hd::tl ->  (List.map (fun x -> hd::x) (extr (i-1) tl)) |> List.append (extr k tl) 
in
extr k l 

let test_extract_2th = 
  let result = extract  2 [1;2;3] in
  let expect = [[2;3];[1;2];[1;3]] in
  (compare result expect) == 0

let () = printf "test_extract_2th: %B\n" (test_extract_2th)
*)

let extract k l = 
  let rec extr k list =
    if k <= 0 then [[]]
    else match list with
      | [] -> []
      | hd :: tl ->
          let with_hd = List.map (fun l -> hd :: l) (extr (k - 1) tl) in
          let without_hd = extr k tl in
          with_hd @ without_hd
    in 
    extr k l

let test_extract_1th = 
  let result = extract  1 [1;2;3] in
  let expect = [[1];[2];[3]] in
  (compare result expect) == 0

let () = printf "test_extract_1th: %B\n" (test_extract_1th)


let test_extract_3th = 
  let result = extract  3 [1;2;3] in
  let expect = [[1;2;3]] in
  (compare result expect) == 0

let () = printf "test_extract_3th: %B\n" (test_extract_3th)

let test_extract_4th = 
  let result = extract  3 [1;2;3] in
  let expect = [[1;3;2]] in
  (compare result expect) != 0

let () = printf "test_extract_4th: %B\n" (test_extract_4th)

(* 27. Group the elements of a set into disjoint subsets. (medium)
In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups. 

Two extract combine
*)

(* let extract k l = 
  let rec extr rest k = function
  | [] -> []
  | _ when k = 0 -> []
  | hd::tl when k = 1 -> ([hd],rest@tl)::(extr (hd::rest) 1 tl)
  | hd::tl -> List.map (fun x -> hd::(fst x),(snd x)) (extr rest (k-1) tl) |> List.append (extr (hd::rest) k tl)
  in 
  extr [] k l

let group l kl = 
  let rec grp l = function
  | [] -> [[]]
  | k::tl -> 
      extract k l |> List.map (fun (cs,rest) -> grp rest tl |> List.map (fun y -> cs::y)) |> List.flatten
  in
  grp l kl  *)


(* 28. Sorting a list of lists according to length of sublists. (medium)
We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later. *)

(* 31. Determine whether a given integer number is prime. (medium) *)
(* https://en.wikipedia.org/wiki/Prime_number *)

let is_prime :int -> bool 
= fun n ->
  let rec is_not_divisor d = 
    d*d>n || ((n mod d <> 0) && (is_not_divisor (d+1))) 
  in
  n <> 1 && is_not_divisor 2;;

let test_is_prime3 = 
  let result = is_prime 3 in
  let expect = true in
  (compare result expect) == 0

let () = printf "test_is_prime: %B\n" (test_is_prime3)
let test_is_prime4 = 
  let result = is_prime 4 in
  let expect = false in
  (compare result expect) == 0

let () = printf "test_is_prime: %B\n" (test_is_prime4)
let test_is_prime7 =
  let result = is_prime 7 in
  let expect = true in
  (compare result expect) == 0

let () = printf "test_is_prime: %B\n" (test_is_prime7)

(* 32. Determine the greatest common divisor of two positive integer numbers. (medium) *)
(* https://en.wikipedia.org/wiki/Greatest_common_divisor *)

let rec gcd a b = 
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)

let test_gcd =
  let result = gcd 6 12 in
  let expect = 6 in
  (compare result expect) == 0

let () = printf "test_gcd: %B\n" (test_gcd)

let test_gcd2 =
  let result = gcd 20536 7826 in
  let expect = 2 in
  (compare result expect) == 0
let () = printf "test_gcd2: %B\n" (test_gcd2)


(* 33. Determine whether two positive integer numbers are coprime. (easy) 
Two numbers are coprime if their greatest common divisor equals 1. *)
(* https://en.wikipedia.org/wiki/Coprime_integers *)

let is_coprime m n =
  gcd m n = 1 

let test_is_coprime =
  let result = is_coprime 2 3 in
  let expect = true in
  (compare result expect) == 0

let () = printf "test_is_coprime: %B\n" (test_is_coprime)

let test_is_coprime2 =
  let result = is_coprime 2 4 in
  let expect = false in
  assert ((compare result expect) == 0)

(* let () = printf "test_is_coprime2: %B\n" (test_is_coprime2) *)

(* 34. Calculate Euler's totient function phi(m). (medium) *)
let euler_totient m = 
  let rec phi n m = 
    match n with
    |  1 -> 1
    |  x -> (if is_coprime x m then 1 else 0 ) + (phi (n-1) m)
  in
  phi (m-1) m

let () = assert ((compare (euler_totient 10) 4) == 0)
let () = assert ((compare (euler_totient 13) 12) == 0)

(* 35. Determine the prime factors of a given positive integer. (medium) *)
(* https://en.wikipedia.org/wiki/Integer_factorization *)
(* 315 
1 2 3 -> 3,105
1 2 3 -> 3,35
1 2 3 4 5 -> 5,7
1 2 3 4 5 6 7 -> 7,1
is_prime && is_divisor
end 
[3;3;5;7]
*)
let prime_factors n =
  let rec pf acc m p= 
    match m with
    | 1 -> acc
    | m -> if m mod p = 0 && is_prime p then pf (p::acc) (m / p) 1 else pf acc m (p + 1)
  in 
  List.rev (pf [] n 1)

let () = assert (compare (prime_factors 315) [3;3;5;7] == 0);;
let () = assert (compare (prime_factors 10) [2;5] == 0);;

(* 36. Determine the prime factors of a given positive integer (2). (medium)
Construct a list containing the prime factors and their multiplicity. Hint: The problem is similar to problem Run-length encoding of a list (direct solution). *)

(* sorted list, accumulate, match *)

(* let encode2 lst = 
  let rec aux acc prev count lst = 
  match prev,count,lst with 
  | _,(x,n),[] -> acc@[(x,n)]
  | p,(x,n),hd::tl -> if (compare p hd)==0
    then aux acc hd (hd,(n+1)) tl 
    else aux (acc@[(x,n)]) hd (hd,1) tl in
  aux [] (List.hd lst) (List.hd lst,0) lst;; *)
let encode_swap lst = 
  let rec iter acc lst = 
    match lst with
    | []->acc
    | (n,x)::tl ->iter ((x,n)::acc) tl 
  in
  List.rev (iter [] lst);;

let prime_factors_count n =
  let rec pf acc m p= 
    match m with
    | 1 -> acc
    | m -> if m mod p = 0 && is_prime p then pf (p::acc) (m / p) 1 else pf acc m (p + 1)
  in 
  (pf [] n 1) |> List.rev |> encode |> encode_swap;;

let () = assert (compare (prime_factors_count 315) [(3,2);(5,1);(7,1)] == 0);;
let () = assert (compare (prime_factors_count 13) [(13,1)] == 0);;

(* 37. Calculate Euler's totient function φ(m) (improved). (medium)
See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula: *)
(* item =(p_x,m_x) product = p_x^m_x *)
let rec power m n =
  match  n with
  | 0 -> 1
  | 1 -> m
  | _ -> m * (power m (n-1));;

let item (p,m) = 
  (p-1) * (power p (m-1));;
  
let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  (* fold_right
  | x::xs -> f x (accumulate f acc xs);; *)
  (* fold_left *)
  | x::xs -> accumulate f (f acc x ) xs

let product_series lst = 
  accumulate ( * ) 1 lst;;

let euler_totient_improved m =
  (* todo 如何对单个item进行管道处理而不是对整个list处理后向后传递 *)
 (prime_factors_count m) |> List.map item|> product_series

let () = assert (compare (euler_totient_improved 10) 4 == 0);;
let () = assert (compare (euler_totient_improved 13) 12 == 0);;

let timeit f a =
  let t0 = Unix.gettimeofday() in
    ignore (f a);
  let t1 = Unix.gettimeofday() in
    t1 -. t0;;

let () =printf  "timeit euler_totient 10090: %F\n" (timeit euler_totient 10090);;
let () =printf  "timeit euler_totient_improved 10090: %F\n" (timeit euler_totient_improved 10090);;

(* 39. A list of prime numbers. (easy)
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. *)

(*  todo generator + filter + Predicate *)
let all_primes low high =
  let rec iter acc n = 
    let cp = compare n high in
    match cp with
    | 1 -> acc 
    | _ -> iter (if is_prime n then (n::acc)else acc) (n+1) in
  iter [] low |> List.rev

let () = assert (compare (List.length (all_primes 2 7920)) 1000 == 0)

let goldbach_conjecture n = 
  let primes = all_primes 2 n in
  let rec find_pair lst rlst=
  match lst,rlst with 
  | [],_ -> failwith "no pair"
  | _,[] -> failwith "no pair"
  | x::xs,y::ys -> 
    if (x+y==n) then (x,y) 
    else if (x+y>n) then find_pair lst ys
    else find_pair xs rlst 
    in 
    find_pair primes (List.rev primes)

let () = assert (compare (goldbach_conjecture 28) (5,23) == 0);;
let () = assert (compare (goldbach_conjecture 13) (2,11) == 0);;
let () = assert (compare (goldbach_conjecture 5) (2,3) == 0);;

let is_even n =
  n mod 2 == 0;;

let goldbach_compositions low high=
  let rec iter acc n = 
    let cp = compare n high in
    match cp with
    | 1 -> acc 
    | _ -> iter (if is_even n then ((n,goldbach_conjecture(n))::acc)else acc) (n+1) in
  iter [] low |> List.rev

let () = assert (compare 
(goldbach_compositions 9 20)
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));(20, (3, 17))]
== 0)
    
(* ------------------------- *)
(* 46 & 47. Truth tables for logical expressions (2 variables). (medium) *)

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec eval a value_a b value_b = function
  | Var x when x = a -> value_a
  | Var x when x = b -> value_b
  | Var _ -> failwith "undefined variable"
  | Not expr -> not (eval a value_a b value_b expr)
  | And (expr1,expr2) -> (eval a value_a b value_b expr1) && (eval a value_a b value_b expr2)
  | Or (expr1,expr2) -> (eval a value_a b value_b expr1) || (eval a value_a b value_b expr2);;

let table2 a b expr = 
  [
    (true,true,eval a true b true expr),
    (true,false,eval a true b false expr),
    (false,true,eval a false b true expr),
    (false,false,eval a false b false expr)
  ];;
  
let _ = table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;

(* 48. Truth tables for logical expressions. (medium) *)

(* https://ocaml.org/api/List.html#:~:text=Since%204.12.0-,Association%20lists,-val%20assoc%20%3A%20%27a 
[(k,v),(k,v)]中通过k找到v的方法。虽然是O(n)，但是hashtable要怎么做pattern matching？ 获取剩余的kv对
*)
(* let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if compare a x = 0 then b else assoc x l *)

let rec eval val_vars = function
  | Var x -> List.assoc x val_vars
  | Not e -> not (eval val_vars e)
  | And (e1,e2) -> (eval val_vars e1) && (eval val_vars e2)
  | Or (e1,e2) -> (eval val_vars e1) || (eval val_vars e2);;

let rec table_make val_vars vars expr =
  match vars with
  | [] -> [List.rev val_vars, eval val_vars expr]
  | v::tl -> 
      table_make ((v,true) :: val_vars) tl expr 
    @ table_make ((v,false):: val_vars) tl expr;;

let table vars expr = table_make [] vars expr;;

(* 49. Gray code. (medium) *)

(* https://ocaml.org/api/Buffer.html#:~:text=encoding%20of%20integers-,Module%20Buffer,-module%20Buffer%3A%20sig 
使用buffer可以避免拼接字符串更高效节约内存，避免内存拷贝和新建字符串。
*)
let string_of_list l =
  let buf = Buffer.create 8 in
  let rec sol = function
  | [] -> Buffer.contents buf
  | hd::tl -> (Buffer.add_string buf (string_of_int hd); sol tl)
  in sol l;;

let gray n =
  let rec  gray' acc i =
  if i > n then acc 
  else gray' ((List.map (fun x->0::x) acc)@(List.map (fun x->1::x) acc)) (i+1) in
  gray' [[0];[1]] 2 |> List.map string_of_list;;

let () = assert (compare (gray 3) ["000";"001";"010";"011";"100";"101";"110";"111"] == 0);;

(* 50. Huffman code (hard) *)
(* https://en.wikipedia.org/wiki/Huffman_coding
https://zh.wikipedia.org/wiki/%E9%9C%8D%E5%A4%AB%E6%9B%BC%E7%BC%96%E7%A0%81#%E6%BC%94%E7%AE%97%E9%81%8E%E7%A8%8B *)
(* Huffman Tree *)
  (* s symbol f frequency l left r right k rank *)
type 'a hf = Empty | Node of 'a * int * 'a hf * 'a hf * int;; 
let mark k= function
| Empty -> Empty
| Node (s,f,l,r,_) -> Node (s,f,l,r,k);;
let rec merge n1 n2=
  match n1,n2 with
  | Empty, Node (s2,f2,_,_,_) -> Node (s2,f2,n1,n2,-1)
  | Node (s1,f1,_,_,_), Empty -> Node (s1,f1,n1,n2,-1)
  | Node (_,f1,_,_,_),Node(_,f2,_,_,_) when f1 > f2 -> merge n2 n1 
  | Node (s1,f1,_,_,_),Node(_,f2,_,_,_) -> Node (s1,f1+f2,(mark 0 n1),(mark 1 n2),-1) 
  | _,_ -> failwith "merge error";;
(* compare *)
let cmp n1 n2 = 
  match n1,n2 with
  | Node (_,f1,_,_,_), Node (_,f2,_,_,_) -> f1 - f2
  | _ -> failwith "cmp error";;
(* sort *)
let sort arr= List.sort cmp arr;;
  (* lst [(s0,f0);(s1;f1);(s2;f2)] *)
let huffman_nodes lst = 
  let trans = function
  | (s,f) -> Node (s,f,Empty,Empty,0) 
  in
  List.map trans lst;;
let build_hf lst = 
  let rec bhf lst = 
    match lst with
    | [] -> Empty
    | [root] -> root
    | h1::h2::tl -> bhf ((merge h1 h2)::tl |>sort) in
    bhf (lst|> huffman_nodes |>sort);;
(* Traversal *)
let trav hf =
  let rec trav' acc path hf = 
  match hf with
  | Empty -> [] 
  | Node (s,_,Empty,Empty,k) -> (s,k::path)::acc
  | Node (_,_,l,r,k) -> trav' acc (k::path) l @ trav' acc (k::path) r in
  trav' [] [] hf;;
let string_of_list_ingore_sentinel l =
  let buf = Buffer.create 8 in
  let rec sol = function
  | [] -> Buffer.contents buf
  | [_] -> Buffer.contents buf
  | hd::tl -> (Buffer.add_string buf (string_of_int hd); sol tl)
  in sol l;;
(* integer list to string *)
let string_of_code lst =
  lst|>List.map (fun (x,l)->(x,string_of_list_ingore_sentinel l));;
let huffman lst = 
  build_hf lst |>trav|>string_of_code;;

let () = assert (compare 
(huffman [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)])
[("a", "0"); ("c", "001"); ("b", "101"); ("f", "0011"); ("e", "1011"); ("d", "111")]
== 0);;

(* 55. Construct completely balanced binary trees. (medium) *)
(* https://en.wikipedia.org/wiki/Binary_tree *)

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all = 
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ("x",l,r)::a) all right 
  in
  List.fold_left add_right_tree all left

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree (n / 2 - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 []);;

(* 56. *)
let rec is_mirror t1 t2 = 
  match t1,t2 with
  | Empty, Empty -> true
  | Node (_,l1,r1), Node (_,l2,r2) -> 
    is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false;;

let is_symmetric = function 
  | Empty -> true
  | Node (_,l,r) -> is_mirror l r;; 

(* 57 https://en.wikipedia.org/wiki/Binary_search_tree*)
(* 构建一个持续添加的结构， insert + fold  *)
let rec insert t x =
  match t with
  | Empty -> Node (x,Empty,Empty)
  | Node (y,l,r) -> 
    if x = y then t
    else if x < y then Node (y,insert l x,r)
    else Node (y,l,insert r x);;
  
let construct l= List.fold_left insert Empty l;;

let () = assert (compare (is_symmetric (construct [5; 3; 18; 1; 4; 12; 21])) true == 0);;

(* 58. Generate-and-test paradigm. (medium) *)
let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n);;

let () = assert (compare
  (List.length (sym_cbal_trees 57))
  (256)
  == 0);;


let rec hbal_tree n =
  if n == 0 then [Empty] 
  else if n =  1 then [Node ("x",Empty,Empty)]
  else 
    let t1 = hbal_tree (n-1) in
    let t2 = hbal_tree (n-2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []))

(* 60. Construct height-balanced binary trees with a given number of nodes. (medium) *)

(* 61. Count the leaves of a binary tree. (easy) *)
let rec count_leaves = function 
| Empty -> 0 
| Node (_,Empty,Empty) -> 1
| Node (_,l,r) -> (count_leaves l) + (count_leaves r);;

let ()  = assert (compare 
  (count_leaves Empty) 
  (0)
  == 0);;

let () = assert (compare 
  (count_leaves (Node('x', Node ('x',Empty,Empty), Node ('x',Empty,Empty))))
  (2)
  == 0);;

(* 61A. Collect the leaves of a binary tree in a list. (easy) *)
let leaves t = 
  let rec leaves' acc t = match t with
    | Empty -> []
    | Node (x,Empty,Empty) -> x::acc
    | Node (_,l,r) -> leaves' (leaves' acc r) l in
    leaves' [] t

let () = assert (compare
  (leaves (Node('x', Node ('x',Empty,Empty), Node ('x',Empty,Empty))))
  (['x';'x'])
  == 0);;

(* 62. Collect the internal nodes of a binary tree in a list. (easy) *)
let internals t = 
  let rec internals' acc t = match t with
    | Empty -> []
    | Node (x,Empty,Empty) -> x::acc
    | Node (x,l,r) ->x:: internals' (internals' acc r) l in
    internals' [] t

  let () = assert (compare
  (internals (Node('x', Node ('x',Empty,Empty), Node ('x',Empty,Empty))))
  (['x';'x';'x'])
  == 0);;

(* 62B. Collect the nodes at a given level in a list. (easy) *)
let at_level t n = 
  let rec at_level' acc t n = match t,n with
    | _,0 -> []
    | Empty,_ -> []
    | Node (x,_,_),1-> x::acc
    | Node (_,l,r),n-> at_level' (at_level' acc r (n-1)) l (n-1) 
  in
  at_level' [] t n;;

let () = assert (compare
(at_level (Node('x', Node ('y',Empty,Empty), Node ('z',Empty,Empty))) 1)
(['x'])
== 0);;
let () = assert (compare
(at_level (Node('x', Node ('y',Empty,Empty), Node ('z',Empty,Empty))) 2)
(['y';'z'])
== 0);;

(* --------------------------------------------- *)

type 'a mult_tree = T of 'a * 'a mult_tree list;;
(* 70C. Count the nodes of a multiway tree. (easy) *)

let rec count_nodes t  =
match t with
| T (_,lst) -> accumulate (fun n t -> n + (count_nodes t)) 1 lst;;

let () = assert (compare
(count_nodes (T ('a', [T ('f', []) ])))
(2)
==0);;

(* 70. Tree construction from a node string. (medium) *)
(* 49. 中使用了Buffer处理字符串拼接 
let string_of_list l =
  let buf = Buffer.create 8 in
  let rec sol = function
  | [] -> Buffer.contents buf
  | hd::tl -> (Buffer.add_string buf (string_of_int hd); sol tl)
  in sol l;; *)

(* Preorder traversal *)
(* let list_of_tree (T (_,sub)) =
  accumulate (fun acc t -> t::acc) [] sub;;

let () = assert (compare
(list_of_tree (T ('a', [T ('f', [T ('g', [])]); T ('c', []);
T ('b', [T ('d', []); T ('e', [])])])))
([])
==0);; *)

(* let rec add_string_of_tree buf (T (c,sub)) = 
  Buffer.add_char buf c;
  List.iter (add_string_of_tree buf) sub;
  Buffer.add_char buf '^'

let string_of_tree t = 
  let buf = Buffer.create 128 in
  add_string_of_tree buf t;
  Buffer.contents buf;; *)


(* 71. Determine the internal path length of a tree. (easy) *)
let internal_path_length t= 
let rec foo deep t =
  match t with
  (* 每递归下一层+ 1 *)
  | T (_,lst) -> accumulate (fun n t -> n + (foo (deep + 1) t)) deep lst
in foo 0 t;;

let () = assert (compare
(internal_path_length (T ('a', [T ('f', [T ('g', [])]); T ('c', []);
T ('b', [T ('d', []); T ('e', [])])])))
(9)
==0);;

(* 72. Construct the bottom-up order sequence of the tree nodes. (easy) *)
(* https://stackoverflow.com/a/24370977/10217249 *)
let bottom_up t =
  let rec foo t l = match t with
    | T (x,lst) -> List.fold_right (fun t acc -> foo t acc) lst (x::l)
  in
  foo t [];;

  let () = assert (compare
  (bottom_up (T ('a', [T ('f', [T ('g', [])]); T ('c', []);
  T ('b', [T ('d', []); T ('e', [])])])))
  (['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a'])
  ==0);;

(* Graphs *)
(* ocaml 的类型定义不支持嵌套嘛 *)
type 'a edge = ('a * 'a);;
(* [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')] *)
type 'a edge_clause = ('a * 'a) list;;
(* {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]} *)
type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;
(* https://www.boost.org/doc/libs/1_37_0/libs/graph/doc/adjacency_list.html *)
(* [('h',['g']);('k',['f']);('f',['b','c']);('c',['b'])] *)
type 'a adjacency_list = ('a * ('a list)) list;;
(* "b-c f-c g-h d f-b k-f h-g" *)
type human_friendly = string;;

(* 80. Conversions. (easy) *)
let neighbors g a = 
  let edges= g.edges in 
  let rec edge l  edges= 
  match edges with
  | [] -> l
  | (x,y)::tl ->
    if x = a then edge (y::l)  tl 
    else if y = a then edge (x::l) tl
    else edge l tl in 
    edge [] edges;;
let term2adjacency g = 
  List.map (fun c -> (c,neighbors g c)) g.nodes;;

let split_edges (elem:(char * char list))= 
  match elem with
  | (_,[]) -> []
  | (x,xs) -> List.map (fun y -> (x,y)) xs;;

let term_append (terms:(char * char)list) term = 
  match term with
  | (x,y) -> 
    if List.mem (x,y) terms || List.mem (y,x) terms then terms
    else (x,y)::terms;;

let adj2terms g= 
    let rec iter acc g = 
      match g with
      | [] -> acc
      | hd::tl -> iter (acc@(split_edges hd)) tl in
      iter [] g;;

let adjacency2term g = 
  let rec iter acc terms = 
    match terms with
    | [] -> acc
    | hd::tl -> iter (term_append acc hd) tl in
  iter [] (adj2terms g);;


(* 81. Path from one node to another one. (medium) *)


let rec list_path g (a:char) (to_b : char list) : char list list= 
  match to_b with
  | [] -> assert false
  | x::_ -> 
    if x = a then [to_b]
    else 
      let n = neighbors g x in 
      let a_n =List.filter (fun c -> (not (List.mem c to_b))) n in 
      List.concat (List.map (fun c -> list_path g a (c :: to_b)) a_n);;

let paths g (a:char) (b:char) = 
  list_path g a [b];;

let () = assert (compare
(paths {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]}'f' 'c')
([['f'; 'b'; 'c']; ['f'; 'c']]) == 0);;


(* Find all possible paths from one node to another in Graph
 https://www.youtube.com/watch?v=TrEloQBv7WQ *)


(* 82. Cycle from a given node. (easy) *)
(* 从该顶点到邻接定点的所有路径 append 该顶点 *)
let cycles (g: 'a graph_term) (a:char) : (char list) list =
  let n = neighbors g a in
  let paths = List.concat (List.map (fun c -> list_path g a [c]) n) in
  List.map(fun p -> p @ [a]) paths;;

(* 83. Construct all spanning trees. (medium) *)
(* https://en.wikipedia.org/wiki/Spanning_tree *)
(* https://zh.wikipedia.org/zh-sg/%E7%94%9F%E6%88%90%E6%A0%91%E5%8D%8F%E8%AE%AE *)
(* https://zh.wikipedia.org/wiki/%E7%94%9F%E6%88%90%E6%A0%91 *)
(* https://stackoverflow.com/questions/2935754/all-minimum-spanning-trees-implementation *)
(* http://people.csail.mit.edu/rivest/mst.py *)
(* https://www.scielo.br/j/pope/a/XHswBwRwJyrfL88dmMwYNWp/?format=pdf&lang=en *)
(* let s_tree g =  *)

(* 84. Construct the minimal spanning tree. (medium) *)

(* 85. Graph isomorphism. (medium) *)

(* 86. Node degree and graph coloration. (medium) *)

(* 87. Depth-first order graph traversal. (medium) *)

(* 88. Connected components. (medium) *)

(* 89. Bipartite graphs. (medium) *)

(* 90. Generate K-regular simple graphs with N nodes. (hard) *)

(* Miscellaneous Problems *)

(* 91. Eight queens problem. (medium) *)
(* structure data->split M task -> [result1,....reesultM] *)
let matrix m n = 
  let range_m = range 1 m in
  let range_n = range 1 n in
  List.map (fun x -> List.map (fun y -> (x,y)) range_n) range_m;;
let board n = 
  matrix n n;;
let is_safe (x1,y1) (x2,y2) = 
  x1 <> x2 && y1 <> y2 && x1-x2 <> y1-y2 && x1-y2 <> x2-y1;;
let check p placed =  
  List.for_all (fun x -> (is_safe p) x) placed;;
let solve n = 
  let rec sols chess_board placed =
    match chess_board with
    | [] -> [placed]
    | row::rows -> 
      let safe_positions = List.filter (fun p -> check p placed) row in
      List.concat (List.map (fun p -> sols rows (p::placed)) safe_positions) in
  sols (board n) [] ;;


(* 92. Knight's tour. (medium) *)
(* https://en.wikipedia.org/wiki/Knight%27s_tour *)
(* https://zh.wikipedia.org/wiki/%E9%A8%8E%E5%A3%AB%E5%B7%A1%E9%82%8F *)
let board n = 
  Array.make_matrix n n 0;;

(* 
structure all path if one Meet the criteria, stop search. => early return
 *)
let knight_moves = 
  [(2,1);(1,2);(-1,2);(-2,1);(-2,-1);(-1,-2);(1,-2);(2,-1)];;
let add (x,y) (v_x,v_y) = 
  (x+v_x,y+v_y);;
let is_on_board (x,y) = 
  x>=1 && x<=8 && y>=1 && y<=8;;
let moves (x,y) = 
  List.filter is_on_board (List.map (fun p -> add (x,y) p) knight_moves);;
let allow_moves p placed = 
  List.filter (fun b -> not (List.mem b placed)) (moves p);;
let comp solution p1 p2 =
  List.length (allow_moves p1 solution) - List.length (allow_moves p2 solution);;
let sorted_moves solution = 
  List.sort (comp solution) (allow_moves (List.hd solution) (List.tl solution));;
let rec do_until f = function
  | [] -> []
  | hd::tl -> match f hd with
    | [] -> do_until f tl
    | answer -> answer;;
let rec extend1 start len soln =
  if (len == 64)
    then soln
  else
    do_until (fun b -> extend1 start (len+1) (b::soln)) (sorted_moves soln);;
let extend start = extend1 start 1 [start];;

(* 93. Von Koch's conjecture. (hard) *)

(* 94. An arithmetic puzzle. (hard) *)

(* 95. English number words. (medium) *)
let n2l n =
  let rec iter aucc n =
    match n with
    | 0 -> aucc
    | x -> iter ((x mod 10)::aucc) (x/10) 
  in
  iter [] n;;

let digit_alist = 
  [
    0,"zero";1,"one";2,"two";3,"three";4,"four";
    5,"five";6,"six";7,"seven";8,"eight";9,"nine";
  ];;

let n2w n =
  let rec iter n alist = 
    match alist with
    | [] -> failwith "error digit"
    | (x,y)::tl -> if x == n then y else iter n tl 
  in
  iter n digit_alist;;

let full_number n  = 
  let dlist = n2l n in
  let wlist = List.map n2w dlist in
  String.concat "-" wlist;;

(* 96. Syntax checker. (medium) *)
(* i.e. which are purely recursive.
 Not fully meet the requirements, can only be said to be barely achieved. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let lexical_analysis char_list =
  let rec iter acc flag char_list = 
    match char_list with 
    | [] -> acc
    | hd::tl -> 
      if hd == '-' then iter (acc@[0]) true tl
      else 
        if flag == true then iter (acc@[1]) false tl
        else iter acc false tl
  in
  iter [] true char_list

let encode tokens = 
  let rec iter flag tokens = 
  match flag,tokens with
  | flag,[] -> if flag == 1 then true else false
  | flag,hd :: tl -> if flag <> hd then iter hd tl else false in 
  iter 0 tokens

let checker s =
  s |> explode |> lexical_analysis |> encode 

(* 97. Sudoku. (medium) *)
module Board = struct
  type t = int array (* 9×9, row-major representation.  A value of 0
                        means undecided. *)

  let is_valid c = c >= 1

  let get (b : t) (x, y) = b.(x + y * 9)

  let get_as_string (b : t) pos =
    let i = get b pos in
    if is_valid i then string_of_int i else "."

  let with_val (b : t) (x, y) v =
    let b = Array.copy b in
    b.(x + y * 9) <- v;
    b

  let of_list l : t =
    let b = Array.make 81 0 in
    List.iteri (fun y r -> List.iteri (fun x e ->
      b.(x + y * 9) <- if e >= 0 && e <= 9 then e else 0) r) l;
    b

  let print b =
    for y = 0 to 8 do
      for x = 0 to 8 do
        printf (if x = 0 then "%s" else if x mod 3 = 0 then " | %s"
                else "  %s")  (get_as_string b (x, y))
      done;
      if y < 8 then
        if y mod 3 = 2 then printf "\n--------+---------+--------\n"
        else printf "\n        |         |        \n"
      else printf "\n"
    done

  let available b (x, y) =
    let avail = Array.make 10 true in
    for i = 0 to 8 do
      avail.(get b (x, i)) <- false;
      avail.(get b (i, y)) <- false;
    done;
    let sq_x = x - x mod 3 and sq_y = y - y mod 3 in
    for x = sq_x to sq_x + 2 do
      for y = sq_y to sq_y + 2 do
        avail.(get b (x, y)) <- false;
      done;
    done;
    let av = ref [] in
    for i = 1 (* not 0 *) to 9 do if avail.(i) then av := i :: !av done;
    !av

  let next (x,y) = if x < 8 then (x + 1, y) else (0, y + 1)

  (** Try to fill the undecided entries. *)
  let rec fill b ((_, y) as pos) =
    if y > 8 then Some b (* filled all entries *)
    else if is_valid(get b pos) then fill b (next pos)
    else match available b pos with
         | [] -> None (* no solution *)
         | l -> try_values b pos l
  and try_values b pos = function
    | v :: l ->
       (match fill (with_val b pos v) (next pos) with
        | Some _ as res -> res
        | None -> try_values b pos l)
    | [] -> None
end

let sudoku b = match Board.fill b (0, 0) with
  | Some b -> b
  | None -> failwith "sudoku: no solution";;
let initial_board =
  Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                  [6; 7; 0;  9; 0; 0;  0; 0; 0];
                  [5; 0; 8;  0; 3; 0;  0; 0; 4];
                  [3; 0; 0;  7; 4; 0;  1; 0; 0];
                  [0; 6; 9;  0; 0; 0;  7; 8; 0];
                  [0; 0; 1;  0; 6; 9;  0; 0; 5];
                  [1; 0; 0;  0; 8; 0;  3; 0; 6];
                  [0; 0; 0;  0; 0; 6;  0; 9; 1];
                  [2; 4; 0;  0; 0; 1;  5; 0; 0]];;
Board.print (sudoku initial_board);;

(* 98. Nonograms. (hard) *)
(* https://link.springer.com/article/10.1007/s10489-009-0200-0 *)
(* https://stackoverflow.com/questions/813366/solving-nonograms-picross *)
(* https://stackoverflow.com/questions/34469538/efficient-nonogram-solver *)
(* https://stackoverflow.com/questions/22966277/evolution-algorithm-to-solve-nonograms *)
(* https://stackoverflow.com/questions/47159012/recursive-algorithm-to-find-all-possible-solutions-in-a-nonogram-row *)
(* https://stackoverflow.com/questions/67180793/implementing-an-algorithm-to-check-a-nonogram-solution *)

(* 99. Crossword puzzle. (hard) *)
