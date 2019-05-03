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

  tree_to_code "" (build_tree fs_pqueue);;

(*
 * Binary trees
 **)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* 55. completely balanced trees *)

let rec cbal_tree node_num =
  let build_variants l r =
    let variants = List.map (fun ltree -> List.map (fun rtree -> Node ('x', ltree, rtree)) r) l
    in
    List.fold_left List.append [] variants
  in
  match node_num with
  | 0 -> [Empty]
  | 1 -> [Node ('x', Empty, Empty)]
  | n when n mod 2 = 0 ->
     let t1 = cbal_tree (n / 2 - 1) in
     let t2 = cbal_tree (n / 2) in
     build_variants t1 t2 @ build_variants t2 t1
  | n ->
     let ts = cbal_tree (n / 2) in
     build_variants ts ts;;

(* 56.  *)

let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) ->
     let rec is_mirror left right =
       match left, right with
       | Node (_, ll, lr), Node (_, rl, rr) -> is_mirror ll rr && is_mirror lr rl
       | Empty, Empty -> true
       | _ -> false
     in
     is_mirror l r;;

(* 57. *)

let construct values =
  let rec push value tree = match tree with
    | Empty -> Node (value, Empty, Empty)
    | Node (node_val, l, r) ->
       if value = node_val then tree
       else if value < node_val then Node (node_val, push value l, r)
       else Node (node_val, l, push value r)
  in
  let rec aux btree = function
    | h::tl -> aux (push h btree) tl
    | [] -> btree
  in
  aux Empty values;;

(* 58. *)

let sym_cbal_trees node_num =
  List.filter is_symmetric (cbal_tree node_num);;

(* 59. *)

let rec hbal_tree height =
  let build_variants l r =
    let variants = List.map (fun ltree -> List.map (fun rtree -> Node ('x', ltree, rtree)) r) l
    in
    List.fold_left List.append [] variants
  in
  match height with
  | 0 -> [Empty]
  | 1 -> [Node ('x', Empty, Empty)]
  | n ->
     let t1 = hbal_tree (n - 1) in
     let t2 = hbal_tree (n - 2) in
     build_variants t1 t1 @ build_variants t1 t2 @ build_variants t2 t1;;

(* 60. TODO*)

(* let max_nodes h = 1 lsl h - 1;;
 *
 * let rec min_nodes h = match h with
 *   | 0 -> 0
 *   | 1 -> 1
 *   | n -> 1 + min_nodes (n - 1) + min_nodes (n - 2);;
 *
 * let min_height n =
 *   int_of_float(ceil(log(float(n + 1)) /. log 2.));;
 *
 * let max_height n =
 *   let rec max_height_search h m_h m_h1 n =
 *   if m_h <= n then max_height_search (h + 1) m_h1 (m_h1 + m_h + 1) n
 *   else h - 1
 *   in
 *   max_height_search 0 0 1 n;;
 *
 * let hbal_tree_nodes n =
 *   let min_h = min_height n and
 *       max_h = max_height n in
 *   let trees = List.map hbal_tree (range min_h max_h) in
 *   List.flatten trees *)

(* 61. *)

let rec count_leaves = function
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
  | Empty -> 0;;

(* 61A. *)

let rec leaves = function
  | Node (v, Empty, Empty) -> [v]
  | Node (_, l, r) -> leaves l @ leaves r
  | Empty -> [];;

(* 62. *)

let rec internals = function
  | Node (v, Empty, Empty) -> []
  | Node (v, l, Empty) -> v::internals l
  | Node (v, Empty, r) -> v::internals r
  | Node (v, l, r) -> v::internals l @ internals r
  | Empty -> [];;

(* 62A. *)

let rec at_level tree n = match n, tree with
  | 1, Node (v, _, _) -> [v]
  | n, Node (v, l, r) -> at_level l (n - 1) @ at_level r (n - 1)
  | _ -> [];;

(* 63. *)

let complete_binary_tree lst =
  let rec aux i lst n =
    if i < n then
      Node (List.nth lst i, aux (2 * i + 1) lst n, aux (2 * i + 2) lst n)
    else Empty
  in
  aux 0 lst (List.length lst);;

(* 64. *)

let layout_binary_tree_1 tree =
  let rec aux startp h = function
    | Node (v, l, r) ->
       let lt, lp = aux startp (h + 1) l in
       let thisp = lp + 1 in
       let rt, rp = aux thisp (h + 1) r in
       Node ((v, thisp, h), lt, rt), rp
    | Empty -> Empty, startp
  in
  aux 0 1 tree;;

(* 65. *)

let layout_binary_tree_2 tree =
  let rec tree_depth = function
    | Node (_, l, r) -> 1 + max (tree_depth l) (tree_depth r)
    | Empty -> 0
  in
  let rec tree_start w = function
    | Node (_, l, r) -> w + tree_start (w / 2) l
    | Empty -> 0
  in
  let rec aux level_width startp h = function
    | Node (v, l, r) ->
       let lt, lp = aux (level_width / 2) (startp - level_width) (h + 1) l in
       let rt, rp = aux (level_width / 2) (startp + level_width) (h + 1) r in
       Node ((v, startp, h), lt, rt), rp
    | Empty -> Empty, startp
  in
  let level_width = int_of_float (2.0 ** (float_of_int ((tree_depth tree) - 1))) / 2
  in
  let root_x = tree_start level_width tree
  in
  aux level_width root_x 1 tree;;

(* 66. TODO *)

(* 67. *)

let rec string_of_tree = function
  | Node (v, Empty, Empty) -> (String.make 1 v)
  | Node (v, l, r) -> (String.make 1 v) ^ "(" ^ (string_of_tree l) ^ "," ^ (string_of_tree r) ^")"
  | Empty -> "";;

let tree_of_string s =
  let explode str =
    let rec explode_inner cur_index chars =
      if cur_index < String.length str then
        let new_char = str.[cur_index] in
        explode_inner (cur_index + 1) (chars @ [new_char])
      else chars in
    explode_inner 0 [] in
  let consume c = function
    | c::rest -> rest
    | _ -> raise (Invalid_argument "Invalid character")
  in
  let rec parse_tree = function
    | [] -> Empty, []
    | ','::rest | ')'::rest as all -> Empty, all
    | c::'('::rest ->
       let l, rest = parse_tree rest in
       let rest = consume ',' rest in
       let r, rest = parse_tree rest in
       Node (c, l, r), consume ')' rest
    | c::rest -> Node (c, Empty, Empty), rest
  in
  let t, _ = parse_tree (explode s) in t;;

(* 68. *)

let rec preorder = function
  | Empty -> []
  | Node (v, l, r) ->  [v] @ preorder l @ preorder r;;

let rec inorder = function
  | Empty -> []
  | Node (v, l, r) -> inorder l @ [v] @ inorder r;;

(* NOTE: Cheated here *)
let rec pre_in_tree preo ino =
  let rec split_pre_in p i x accp acci = match (p, i) with
    | [], [] -> (List.rev accp, List.rev acci), ([], [])
    | h1::t1, h2::t2 ->
       if x=h2 then
         (List.tl (List.rev (h1::accp)), t1),
         (List.rev (List.tl (h2::acci)), t2)
       else
         split_pre_in t1 t2 x (h1::accp) (h2::acci)
    | _ -> assert false in
  match (preo, ino) with
  | [], [] -> Empty
  | v1::rest1, v2::rest2 ->
     let (lp, rp), (li, ri) = split_pre_in preo ino v1 [] [] in
     Node (v1, pre_in_tree lp li, pre_in_tree rp ri)
  | _ -> invalid_arg "pre_in_tree";;

(* 69. TODO *)

(*
 * Multiway Trees
 **)

type 'a mult_tree = T of 'a * 'a mult_tree list;;

(* 70C. *)

let rec count_nodes (T (_, children)) =
  1 + List.fold_left (+) 0 (List.map count_nodes children);;

(* 70. *)

let rec string_of_tree (T (c, children)) =
  String.make 1 c ^
    List.fold_left (^) "" (List.map (fun child -> (string_of_tree child) ^ "^") children);;

(* NOTE: copypaste below :-( *)
let rec tree_of_substring t s i len =
    if i >= len || s.[i] = '^' then List.rev t, i + 1
    else
      let sub, j = tree_of_substring [] s (i+1) len in
      tree_of_substring (T(s.[i], sub) ::t) s j len;;

let tree_of_string s =
  match tree_of_substring [] s 0 (String.length s) with
  | [t], _ -> t
  | _ -> failwith "tree_of_string";;

(* 71. *)

let ipl t =
  let rec aux path (T (_, children)) =
    let children_paths = List.map (aux (path + 1)) children in
    path + List.fold_left (+) 0 children_paths
  in
  aux 0 t;;

(* 72. *)

let rec prepend_bottom_up (T(c, sub)) l =
  List.fold_right prepend_bottom_up sub (c :: l);;
let bottom_up t = prepend_bottom_up t [];;

(* 73. *)

let rec lispy = function
  | T (c, []) -> String.make 1 c
  | T (c, children) ->
     let children_strings =
       String.concat " " (List.map lispy children)
     in
     "(" ^ String.make 1 c ^ " " ^ children_strings ^ ")";;

(*
 * Graphs
 * *)

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

let example_graph =
  { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] };;

(* 81. *)

let paths graph st nd =
  let rec aux seen curr =
    if curr = nd then [List.rev (curr::seen)]
    else
      let pairs_to_visit =
        List.find_all (fun (s, t) -> s = curr && not (List.mem t seen)) graph.edges
      in
      let nodes_to_visit =
        List.map (fun (s, t) -> t) pairs_to_visit
      in
      List.fold_left (@) [] (List.map (aux (curr::seen)) nodes_to_visit)
  in
  aux [] st;;

(* 82. *)

let cycles graph start =
  let target_nodes_from seen source =
    List.map snd (List.find_all (fun (s, t) -> s = source && (not (List.mem t seen) || t = start)) graph.edges)
    @ List.map fst (List.find_all (fun (t, s) -> s = source && (not (List.mem t seen) || t = start)) graph.edges)
  in
  let rec aux seen curr =
    if List.length seen > 0 && curr = start then List.rev [curr::seen]
    else
      let target_nodes = target_nodes_from seen curr in
      List.fold_left (@) [] (List.map (aux (curr::seen)) target_nodes)
  in
  (* target_nodes_from start [] *)
  aux [] start;;

(* 87. TODO *)

(*
 * Miscellaneous Problems
 **)

(* 91. *)

let queens_positions queens_num =
  let check_row poslist y =
    not (List.mem y poslist)
  in
  let has_diag coords (x, y) =
    let coords = List.filter (fun (_, that_y) -> that_y <> y) coords in
    List.exists (fun (that_x, that_y) -> abs (that_x - x) = abs (that_y - y)) coords
  in
  let check_diags poslist =
    let coords = List.mapi (fun x y -> (x, y)) poslist in
    not (List.exists (fun (x, y) -> has_diag coords (x, y)) coords)
  in
  let rec aux poslist queens_left =
    if queens_left = 0 then [poslist]
    else
      let candidates = List.filter (check_row poslist) (range 0 (queens_num - 1))
      in
      List.fold_left ( @ ) [] (List.map (fun c -> aux (c::poslist) (queens_left - 1)) candidates)
  in
  List.filter check_diags (aux [] queens_num);;

(* 95. *)

let full_words num =
  let digits = [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |] in
  let rec aux num_left acc =
    if num_left > 0 then
      let n = num_left mod 10 in
      aux (num_left / 10) ((digits.(n))::acc)
    else
      acc
  in
  if num = 0 then "zero"
  else String.concat "-" (aux num []);;

(* 96. *)


let identifier ident =
  let is_letter c = 'a' <= c  && c <= 'z' in
  let is_num c = '0' <= c  && c <= '9' in
  let is_alnum c = is_letter c || is_num c in
  let rec alnum i len = match i with
    | i when i = len -> false
    | i when is_alnum ident.[i] -> maybe_negalnum (i + 1) len
    | _ -> false
  and maybe_negalnum i len = match i with
    | i when i = len -> true
    | i when ident.[i] = '-' -> alnum (i + 1) len
    | i -> alnum i len
  and letter i len = match i with
    | i when i = len -> false
    | i when is_letter ident.[i] -> maybe_negalnum (i + 1) len
    | _ -> false
  in
  letter 0 (String.length ident);;
