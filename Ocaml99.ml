open Stdio
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