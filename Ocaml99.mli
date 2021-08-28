val print_n_list : int list -> unit
val print_s_list : string list -> unit
val last : 'a list -> 'a option
val test_last : bool
val last_two : 'a list -> ('a * 'a) option
val test_last_two : bool
val at : int -> 'a list -> 'a option
val test_at : bool
val length : 'a list -> int
val test_length : bool
val rev : 'a list -> 'a list
val test_rev : bool
val rev_two : 'a list -> 'a list
val test_rev_two : bool
val is_palindrome : 'a list -> bool
val test_is_palindrome : bool
type 'a node = One of 'a | Many of 'a node list
val flatten : 'a node list -> 'a list
val test_flatten : bool
val flatten_two : 'a node list -> 'a list
val test_flatten_two : bool
val compress : string list -> string list
val test_compress : bool
val pack : 'a list -> 'a list list
val test_pack : bool
val encode : 'a list -> (int * 'a) list
val test_encode : bool
val encode_two : 'a list -> (int * 'a) list
val test_encode_two : bool
type 'a rle = One of 'a | Many of int * 'a
val encode_3th : 'a list -> 'a rle list
val test_encode_3th : bool
val expend : int -> 'a -> 'a list
val test_expend : bool
val decode : 'a rle list -> 'a list
val test_decode : bool
val duplicate : 'a list -> 'a list
val test_duplicate : bool
val replicate : 'a list -> int -> 'a list
val test_replicate : bool
val drop : 'a list -> int -> 'a list
val test_drop : bool
val split : 'a list -> int -> ('a list * 'a list) list
val test_split : bool
val slice : 'a list -> int -> int -> 'a list
val test_slice : bool
val rotate : 'a list -> int -> 'a list
val test_rotate : bool
val test_rotate_tw0 : bool
val remove_at : int -> 'a list -> 'a list
val test_remove_at : bool
val insert_at : int -> 'a list -> 'a -> 'a list
val test_insert_at : bool
val range : int -> int -> int list
val test_range : bool
val test_range_two : bool
val at2 : int -> 'a list -> 'a
val rand_select : 'a list -> int -> 'a list
val test_rand_select : bool
val lotto_select : int -> int -> int list
val test_lotto_select : bool
val permutation : 'a list -> 'a list
val test_permutation : bool
val foo : 'a list -> 'a list list
val test_foo : bool
val extract : 'a list -> 'a list list
val test_extract : bool
val extract : int -> 'a list -> 'a list list
val test_extract_1th : bool
val test_extract_3th : bool
val test_extract_4th : bool
val is_prime : int -> bool
val test_is_prime3 : bool
val test_is_prime4 : bool
val test_is_prime7 : bool
val gcd : int -> int -> int
val test_gcd : bool
val test_gcd2 : bool
val is_coprime : int -> int -> bool
val test_is_coprime : bool
val test_is_coprime2 : unit
val euler_totient : int -> int
val prime_factors : int -> int list
val encode_swap : ('a * 'b) list -> ('b * 'a) list
val prime_factors_count : int -> (int * int) list
val power : int -> int -> int
val item : int * int -> int
val accumulate : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val product_series : int list -> int
val euler_totient_improved : int -> int
val timeit : ('a -> 'b) -> 'a -> float
val all_primes : int -> int -> int list
val goldbach_conjecture : int -> int * int
val is_even : int -> bool
val goldbach_compositions : int -> int -> (int * (int * int)) list
type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
val eval : string -> bool -> string -> bool -> bool_expr -> bool
val table2 :
  string ->
  string ->
  bool_expr ->
  ((bool * bool * bool) * (bool * bool * bool) * (bool * bool * bool) *
   (bool * bool * bool))
  list
val eval : (string * bool) list -> bool_expr -> bool
val table_make :
  (string * bool) list ->
  string list -> bool_expr -> ((string * bool) list * bool) list
val table : string list -> bool_expr -> ((string * bool) list * bool) list
val string_of_list : int list -> string
val gray : int -> string list
type 'a hf = Empty | Node of 'a * int * 'a hf * 'a hf * int
val mark : int -> 'a hf -> 'a hf
val merge : 'a hf -> 'a hf -> 'a hf
val cmp : 'a hf -> 'b hf -> int
val sort : 'a hf list -> 'a hf list
val huffman_nodes : ('a * int) list -> 'a hf list
val build_hf : ('a * int) list -> 'a hf
val trav : 'a hf -> ('a * int list) list
val string_of_list_ingore_sentinel : int list -> string
val string_of_code : ('a * int list) list -> ('a * string) list
val huffman : ('a * int) list -> ('a * string) list
type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree
val count : 'a binary_tree -> int
val is_balanced : 'a binary_tree -> bool
