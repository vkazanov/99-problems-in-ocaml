(*
 * https://ocaml.org/learn/tutorials/99problems.html
 **)

(* 1.  *)
let rec last lst = match lst with
    [] -> None
  | [h] -> Some h
  | _::tl -> last tl;;

(* 2. *)
let rec last_two lst = match lst with
    [] | [_]-> None
  | [h1; h2] -> Some (h1, h2)
  | h1::tl -> last_two tl;;

(* 3. *)
let rec at k lst = match lst with
    [] -> None
  | h::tl -> if k = 0 then Some h else at (k - 1) tl;;

(* 4. *)
let length lst =
  let rec length_aux acc = function
    | [] -> acc
    | _::tl -> length_aux (acc + 1) tl
  in length_aux 0 lst;;

(* 5.  *)
let rev lst =
  let rec rev_aux target = function
    | [] -> target
    | h::tl -> rev_aux (h::target) tl
  in rev_aux [] lst;;

(* 6. *)
let is_palindrome s = s = List.rev s;;

(* 7. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One h :: tl -> aux (h::acc) tl
    | Many elems :: tl -> aux (aux acc elems) tl in
  List.rev (aux [] lst);;

(* 8. *)
let rec compress = function
  | a::(b::_ as t) -> if a = b then compress t
                      else a::compress t
  | smaller -> smaller;;

(* 9. *)
let rec pack lst =
  let rec aux acc = function
    | a::(b::_ as t) -> if a=b then aux (a::acc) t
                        else (a::acc)::aux [] t
    | [a] -> [(a::acc)]
    | [] -> [] in
  aux [] lst;;

(* 10. *)

let encode lst =
  List.map
    (fun l -> List.length l, List.hd l)
    (pack lst);;

(* 11. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode lst =
  List.map
    (function
     | [h] -> One h
     | h::_ as many -> Many (List.length many,  h)
     | _ -> raise (Invalid_argument "Should not get zero length lists"))
    (pack lst);;

(* 12. *)

let decode lst =
  let rec make_elems_list elem = function
    | 1 -> [elem]
    | n -> elem::make_elems_list elem (n - 1)
  in
  let expand = function
    | One elem -> [elem]
    | Many (count, elem) -> make_elems_list elem count
  in
  List.flatten (List.map expand lst);;

(* 13. *)

let encode lst =
  let aux_make_rle elem = function
    | 1 -> One elem
    | n -> Many (n, elem)
  in
  let rec aux acc cur_count = function
    | a::(b::_ as tl) -> if a=b then aux acc (cur_count + 1) tl
                         else aux (aux_make_rle a (cur_count + 1) :: acc) 0 tl
    | [a] -> aux_make_rle a (cur_count + 1) :: acc
    | [] -> []
  in
  List.rev (aux [] 0 lst);;

(* 14. *)

let rec duplicate = function
  | h::tl -> h::h::duplicate tl
  | [] -> [];;

(* 15. *)

let rec replicate lst times =
  let rec repl_elem elem = function
    | 0 -> []
    | n -> elem::repl_elem elem (n - 1)
  in
  match lst with
  | h::tl -> repl_elem h times @ replicate tl times
  | [] -> [];;

(* 16. *)

let drop lst nth =
  let rec aux res i = function
    | h::tl -> if i mod nth = 0
               then aux res (i+1) tl
               else aux (h::res) (i+1) tl
    | [] -> res
  in
  List.rev(aux [] 1 lst);;

(* 17. *)

let split lst l =
  let rec collect acc i = function
    | h::tl as lst -> if i > 0 then collect (h::acc) (i-1) tl
                      else List.rev acc, lst
    | [] -> List.rev acc, []
  in
  collect [] l lst;;

(* 18. *)

let slice lst i k =
  let rec aux res idx = function
    | h::tl -> if idx >= i && idx <= k then aux (h::res) (idx+1) tl
               else if idx < i then aux res (idx+1) tl
               else res
    | [] -> res
  in
  List.rev (aux [] 0 lst);;

(* 19. *)

let rotate lst k =
  let rec rot_left new_tail k = function
    | h::tl as l -> if k > 0 then rot_left (h::new_tail) (k - 1) tl
                    else l @ List.rev new_tail
    | [] -> List.rev new_tail
  in
  let k_mod = k mod List.length lst
  in
  if k >= 0 then rot_left [] k_mod lst
  else List.rev (rot_left [] (-k_mod) (List.rev lst));;

(* 20. *)

let remove_at k lst =
  let rec aux acc i = function
    | h::tl -> if i = k then aux acc (i + 1) tl
               else aux (h::acc) (i + 1) tl
    | [] -> List.rev acc
  in
  aux [] 0 lst;;

(* 21. *)

let insert_at elem k lst =
  let rec aux acc i = function
    | [] -> if i <= k then List.rev (elem::acc)
            else List.rev acc
    | h::tl -> if i <> k then aux (h::acc) (i+1) tl
               else aux (h::elem::acc) (i+1) tl
  in
  aux [] 0 lst;;

(* 22. *)

let range i j =
  let rec aux acc i j =
    if i <= j then aux (i::acc) (i+1) j
    else List.rev acc
  in
  if i <= j then aux [] i j
  else List.rev (aux [] j i);;

(* 23. *)

let rand_select lst num =
  let lst_len = List.length lst in
  let rec rand_aux acc num_left =
    if num_left > 0 then
      let new_elem = List.nth lst (Random.int lst_len)
      in rand_aux (new_elem::acc) (num_left - 1)
    else acc
  in
  rand_aux [] (min num lst_len);;

(* 24. *)

let lotto_select num_to_pick max_num =
  rand_select (range 1 max_num) num_to_pick;;

(* 25. *)

let permutation list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux len [] list len;;

(* 26. *)

let extract k lst =
  let rec aux comb k options =
    if k > 0 then
      match options with
      | [] -> []
      | h::tl ->
         let with_h = aux (h::comb) (k-1) tl in
         let without_h = (aux comb k tl) in
         with_h @ without_h
    else
      [List.rev comb]
  in
  aux [] k lst;;

(* 27. *)

let group lst group_sizes=
  let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
  let append_extracted gs num_to_extract rest =
    List.map (fun extracted -> (extracted::gs, diff rest extracted)) (extract num_to_extract rest)
  in
  let extract_group size groups =
    List.flatten (List.map (fun (gs, rest) ->  append_extracted gs size rest) groups)
  in
  let drop_rest groups =
    List.map (fun (gs, rest) -> List.rev gs) groups
  in
  let rec build_groups groups = function
    | [] -> groups
    | h::tl -> build_groups (extract_group h groups) tl
  in
  drop_rest (build_groups [([], lst)] group_sizes);;

(* 28. *)

let length_sort =
  List.sort (fun lst1 -> fun lst2 -> List.length lst1 - List.length lst2);;

let frequency_sort lst =
  let module IntMap = Map.Make(struct type t = int let compare = compare end)
  in

  let add_or_update lst_to_sort fmap =
    let lst_len = (List.length lst_to_sort) in
    let old_freq = try IntMap.find lst_len fmap with Not_found -> 0 in
    IntMap.add lst_len (old_freq + 1) fmap
  in

  let rec build_freq fmap = function
    | h::tl -> build_freq (add_or_update h fmap) tl
    | [] -> fmap
  in

  let fmap = build_freq IntMap.empty lst
  in

  let lst_to_freq lst_to_sort = IntMap.find (List.length lst_to_sort) fmap
  in

  List.sort (fun lst1 -> fun lst2 -> lst_to_freq lst1 - lst_to_freq lst2) lst;;

(*
 * Arithmetic
 **)

(* 31. *)

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    if d * d > n then true
    else n mod d <> 0 && is_not_divisor (d+1)
  in
  n <> 1 && is_not_divisor 2;;

(* 32. *)

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b);;

(* 33. *)

let coprime a b =
  gcd a b = 1;;

(* 34. *)

let phi m =
  if m = 1 then 1
  else List.length (List.filter (coprime m) (range 1 (m - 1)));;

(* 35. *)

let factors m =
  let rec aux acc d n =
      if n = 1 then acc
      else if n mod d <> 0 then aux acc (d + 1) n
      else aux (d::acc) d (n / d)
  in
  List.rev (aux [] 2 m);;

(* 36. *)
let factors m =
  let rec pack_aux lst =
    let rec aux acc = function
      | a::(b::_ as t) -> if a=b then aux (a::acc) t
                          else (a::acc)::aux [] t
      | [a] -> [(a::acc)]
      | [] -> [] in
    aux [] lst
  in

  let encode_aux lst =
    List.map
      (fun l -> List.hd l, List.length l)
      (pack_aux lst)
  in

  let factors_aux m =
    let rec aux acc d n =
      if n = 1 then acc
      else if n mod d <> 0 then aux acc (d + 1) n
      else aux (d::acc) d (n / d)
    in
    List.rev (aux [] 2 m)
  in

  encode_aux (factors_aux m);;

(* 37. *)

let phi_improved n =
  let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float in
  let prime_factors = factors n in
  let nums_to_fold = List.map (fun (p, m) -> (p - 1) * int_exp p (m - 1)) prime_factors in
  List.fold_left ( * ) 1 nums_to_fold;;

(* 38. *)

(* Need to load that thing. What is it, BTW? *)
#load "unix.cma";;

let timeit f a =
  let open Unix in
  let t_start = Unix.gettimeofday () in
  ignore (f a);
  let t_stop = Unix.gettimeofday () in
  t_stop -. t_start;;

(* 39. *)

let all_primes i j =
  List.filter is_prime (range i j);;

(* 40. *)

let goldbach n =
  let prms = all_primes 2 (n - 1) in
  let find_pair left =
    List.find (fun right -> left + right = n) prms
  in
  let rec check_head = function
    | [] -> raise Not_found
    | h::tl -> try (find_pair h, h)
               with Not_found -> check_head tl
  in
  check_head prms;;

(* 41. *)

let goldbach_list i j =
  let evens = List.filter (function n -> n mod 2 = 0) (range i j) in
  List.map (function n -> (n, goldbach n)) evens;;

let goldbach_limit i j limit =
  let g_lst = (goldbach_list i j) in
  List.filter (function | (n, (l, r)) -> l > limit && r > limit) g_lst;;

(*
 * Logic and codes
 **)

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

(* 46. & 47. *)

let table2 vname1 vname2 expr =
  let module StringMap = Map.Make(String)
  in

  let rec eval var_table = function
    | Var vname -> StringMap.find vname var_table
    | Not expr -> not (eval var_table expr)
    | And (expr1, expr2) -> (eval var_table expr1)
                            && (eval var_table expr2)
    | Or (expr1, expr2) -> (eval var_table expr1)
                           || (eval var_table expr2)
  in

  let table_from_values vname1 val1 vname2 val2 =
    let m = StringMap.add vname1 val1 StringMap.empty in
    StringMap.add vname2 val2 m
  in

  let eval_with_values (v1, v2) =
    (v1, v2, eval (table_from_values vname1 v1 vname2 v2) expr)
  in

  List.map eval_with_values [(true, true); (true, false); (false, true); (false, false)];;


(* 48. *)

let table vars expr =
  let module StringMap = Map.Make(String)
  in

  let rec eval var_table = function
    | Var vname -> StringMap.find vname var_table
    | Not expr -> not (eval var_table expr)
    | And (expr1, expr2) -> eval var_table expr1 && eval var_table expr2
    | Or (expr1, expr2) -> eval var_table expr1 || eval var_table expr2
  in

  let rec build_tables partial_table = function
    | [] -> [partial_table]
    | h::tl ->
       let with_true = build_tables (StringMap.add h true partial_table) tl
       and with_false = build_tables (StringMap.add h false partial_table) tl
       in with_true @ with_false
  in

  let var_tables = build_tables StringMap.empty vars in
  List.map (function vtable -> (StringMap.bindings vtable, eval vtable expr)) var_tables;;


(* 49. *)

let rec gray n =
  let prefix_with code prefix =
    List.map (function bitstr -> prefix ^ bitstr) code
  in
  match n with
  | 1 -> ["0"; "1"]
  | n -> let prev = gray (n - 1) in
         let next = List.rev prev in
         prefix_with prev "0" @ prefix_with next "1";;

(* 50. Huffman code*)

type htree =
  | Leaf of string * int
  | Node of htree * htree * int;;

let huffman fs =
  let hfreq = function
    | Leaf (_, freq) -> freq
    | Node (_, _, freq) -> freq
  in

  let hcompare l r = hfreq l - hfreq r in

  (* Use Set as an improvised priority queue *)
  let module PQSet = Set.Make (struct type t = htree let compare = hcompare end) in

  (* Build a Huffman tree *)
  (* TODO: kinda ugly *)
  let rec build_tree pqueue =
    if PQSet.cardinal pqueue > 1 then
      let l = PQSet.min_elt pqueue in
      let pqueue1 = PQSet.remove l pqueue in
      let r = PQSet.min_elt pqueue1 in
      let pqueue2 = PQSet.remove r pqueue1 in
      let new_node = Node (l, r, hfreq l + hfreq r) in

      build_tree (PQSet.add new_node pqueue2)
    else
      PQSet.min_elt pqueue
  in

  (* Convert the tree to chars mapped to codes *)
  let rec tree_to_code prefix = function
    | Node (l, r, _) -> tree_to_code ("0" ^ prefix) l @ tree_to_code ("1" ^ prefix) r
    | Leaf (c, _) -> [c, prefix]
  in

  (* A PQueue of Leafs *)
  let fs_pqueue = PQSet.of_list (List.map (fun (c, f) -> Leaf (c, f)) fs)
  in

  tree_to_code "" (build_tree fs_pqueue)
