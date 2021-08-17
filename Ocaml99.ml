open Stdio
let rec print_n_list = function 
  [] -> ()
  | e::l -> printf "%N" e ; print_string " " ; print_n_list l
let rec print_s_list = function 
  [] -> ()
  | e::l -> printf "%S" e ; print_string " " ; print_s_list l
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


(* OCaml equivalent of Python generators https://stackoverflow.com/a/28356215/10217249 *)
(* Combination https://en.wikipedia.org/wiki/Combination *)

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