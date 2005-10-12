type t =
    Empty
  | Leaf of int
  | Branch of int * int * int * t * t
    (* Branch (p, m, c, l, r):
       [p] is the largest common prefix for all the keys in this tree
       [m] is the branching bit mask
           ([m] is a power of 2, only the bits above [m] are valid in [p])
       [c] is the number of leaves of the tree (number of elements)
       [l] contains all the keys with a 0 in the branching bit
       [r] contains all the keys with a 1 in the branching bit  *)
;;

let empty = Empty;;
let singleton i = Leaf i;;
let is_empty s = s = Empty;;

let size = function
    Empty -> 0
  | Leaf _ -> 1
  | Branch (_, _, c, _, _) -> c
;;

(* Another nice property of Patricia trees is to be independent of the
   order of insertion. As a consequence, two Patricia trees have the
   same elements if and only if they are structurally equal.  *)
let equal = (=);;

let rec contains s i = match s with
    Empty -> false
  | Leaf j -> j = i
  | Branch (p, _, _, l, r) ->
      if i <= p
      then contains l i
      else contains r i
;;

let rec choose = function
  | Empty -> raise Not_found
  | Leaf i -> i
  | Branch (_, _, _, l,_) -> choose l
;;

(* Return an integer where only the highest bit that was set in [x] is
   still set.  *)
let rec highest_bit x =
  let x' = x land (x - 1) in
    if x' = 0
    then x
    else highest_bit x'
;;

let branching_bit m p1 p2 =
  let x = p1 lxor p2 in			(* remove common prefix *)
  let x = x land lnot (m - 1) in	(* remove invalid suffix *)
    highest_bit x;;

(* In [i], clear the 1-bit mask [m], and set all bits below [m]'s bit to one.  *)
let mask i m = (i lor (m - 1 + m)) - m;;

let prefix_matches i p m = (mask i m = p)

(* Combine two trees with prefixes P1 and P2, where P1 and P2 are
   known to disagree.  *)
let join m p1 s1 p2 s2 c =
  let m = branching_bit m p1 p2 in
  let p = mask p1 m
  in
    if p1 < p2
    then Branch (p, m, c, s1, s2)
    else Branch (p, m, c, s2, s1)
;;

let rec add s i = match s with
    Empty -> Leaf i
  | Leaf j when j = i -> s
  | Leaf j -> join 1 i (Leaf i) j s 2
  | Branch (p, m, c, l, r) ->
      if prefix_matches i p m then
	if i <= p then
	  let l' = add l i in
	    if l' == l
	    then s
	    else Branch (p, m, c + 1, l', r)
	else
	  let r' = add r i in
	    if r' == r
	    then s
	    else Branch (p, m, c + 1, l, r')
      else
	join (m lsl 1) i (Leaf i) p s (c + 1)
;;

let rec remove s i =
  let branch = function
    | (_, _, _, Empty, s) -> s
    | (_, _, _, s, Empty) -> s
    | (p, m, c, l, r) -> Branch (p, m, c, l, r) in
  match s with
    Empty -> Empty
  | Leaf j when j = i -> Empty
  | Leaf _ -> s
  | Branch (p, m, c, l, r) ->
      if prefix_matches i p m then
	if i <= p then
	  let l' = remove l i in
	    if l' == l
	    then s
	    else branch (p, m, c - 1, l', r)
	else
	  let r' = remove r i in
	    if r' == r
	    then s
	    else branch (p, m, c - 1, l, r')
      else
	s
;;

let pop s =
  let i = choose s in
    i, remove s i
;;

let branch p m l r = Branch (p, m, size l + size r, l, r);;

let rec union s1 s2 = match s1, s2 with
    Empty, t  -> t
  | t, Empty  -> t
  | Leaf k, t -> add t k
  | t, Leaf k -> add t k
  | Branch (p1, m1, c1, l1, r1), Branch (p2, m2, c2, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	(* The trees have the same prefix. Merge the subtrees.  *)
	branch p1 m1 (union l1 l2) (union r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	(* [p2] contains [p1]. Merge [s2] with a subtree of [s1].  *)
        if p2 <= p1
	then branch p1 m1 (union l1 s2) r1
        else branch p1 m1 l1 (union r1 s2)
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        (* [p1] contains [p2]. Merge [s1] with a subtree of [s2].  *)
        if p1 <= p2
	then branch p2 m2 (union s1 l2) r2
        else branch p2 m2 l2 (union s1 r2)
      else
	(* The prefixes disagree.  *)
	join (m1 lsl 1) p1 s1 p2 s2 (c1 + c2)
;;

let rec minus s1 s2 = match s1, s2 with
    Empty, _ -> Empty
  | _, Empty -> s1
  | Leaf i, _ -> if contains s2 i then Empty else s1
  | _, Leaf i -> remove s1 i
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	union (minus l1 l2) (minus r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
        if p2 <= p1
	then union (minus l1 s2) r1
        else union l1 (minus r1 s2)
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        if p1 <= p2
	then minus s1 l2
        else minus s1 r2
      else
	s1
;;

let rec is_subset s1 s2 = match s1, s2 with
    Empty, _   -> true
  | _, Empty   -> false
  | Leaf k1, _ -> contains s2 k1
  | Branch _, Leaf _ -> false
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	is_subset l1 l2 && is_subset r1 r2
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        if p1 <= p2
	then is_subset l1 l2 && is_subset r1 l2
        else is_subset l1 r2 && is_subset r1 r2
      else
	false
;;
      
let rec intersection s1 s2 = match s1, s2 with
    Empty, _
  | _, Empty -> Empty
  | Leaf i, _ -> if contains s2 i then s1 else Empty
  | _, Leaf i -> if contains s1 i then s2 else Empty
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	union (intersection l1 l2) (intersection r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	intersection (if p2 <= p1 then l1 else r1) s2
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        intersection s1 (if p1 <= p2 then l2 else r2)
      else
	Empty
;;

let rec do_intersect s1 s2 = match s1, s2 with
    Empty, _
  | _, Empty -> false
  | Leaf i, t
  | t, Leaf i -> contains t i
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
        do_intersect l1 l2 || do_intersect r1 r2
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	do_intersect (if p2 <= p1 then l1 else r1) s2
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        do_intersect s1 (if p1 <= p2 then l2 else r2)
      else
	false
;;

let rec intersection_size s1 s2 = match s1, s2 with
    Empty, _ 
  | _, Empty -> 0
  | Leaf i, _ -> if contains s2 i then 1 else 0
  | _, Leaf i -> if contains s1 i then 1 else 0
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	intersection_size l1 l2 + intersection_size r1 r2
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	intersection_size (if p2 <= p1 then l1 else r1) s2
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        intersection_size s1 (if p1 <= p2 then l2 else r2)
      else
	0
;;

let rec fold_intersection f s1 s2 accu = match s1, s2 with
    Empty, _
  | _, Empty -> accu
  | Leaf i, _ -> if contains s2 i then f accu i else accu
  | _, Leaf i -> if contains s1 i then f accu i else accu
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	fold_intersection f l1 l2 (fold_intersection f r1 r2 accu)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	fold_intersection f (if p2 <= p1 then l1 else r1) s2 accu
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        fold_intersection f s1 (if p1 <= p2 then l2 else r2) accu
      else
	accu
;;

let rec find_opt p = function
    Empty -> None
  | Leaf i -> p i
  | Branch (_, _, _, l, r) ->
      match find_opt p l with
	None -> find_opt p r
      | some -> some
;;

let find p s =
  match find_opt (fun x -> if p x then Some x else None) s with
      None -> raise Not_found
    | Some x -> x
;;

let rec for_all p = function
    Empty -> true
  | Leaf i -> p i
  | Branch (_, _, _, l, r) ->
      for_all p l && for_all p r
;;

let rec iter f = function
    Empty -> ()
  | Leaf i -> f i
  | Branch (_, _, _, l, r) ->
      iter f l;
      iter f r;
;;

let rec fold f s accu = match s with
    Empty -> accu
  | Leaf i -> f accu i
  | Branch (_, _, _, l, r) -> fold f r (fold f l accu)
;;

let split s i =
  fold
    (fun (l, present, r) j ->
      if j < i
      then add l j, present, r
      else if j > i
      then l, present, add r j
      else l, true, r)
    s
    (empty, false, empty)
;;

let partition p s =
  fold
    (fun (t, f) i ->
      if p i
      then add t i, f
      else t, add f i)
    s
    (empty, empty)
;;

let of_list = List.fold_left add empty;;

let output channel s =
  Printf.fprintf channel "{%d:" (size s);
  iter (fun i -> Printf.fprintf channel " %d" i) s;
  Printf.fprintf channel "}";
;;

let print s = output stdout s;;
let dump  s = output stderr s;;
