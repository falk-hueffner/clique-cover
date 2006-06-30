type winner = Left | Right;;
    
type 'a t =
    Empty
  | Leaf of int * 'a * int
  | Branch of int * int * winner * int * 'a t * 'a t
;;

let empty = Empty;;
let is_empty s = s = Empty;;

let rec highest_bit x =
  let x' = x land (x - 1) in
    if x' = 0
    then x
    else highest_bit x'
;;

let branching_bit m p1 p2 =
  let x = p1 lxor p2 in
  let x = x land lnot (m - 1) in
    highest_bit x
;;

let mask i m = (i lor (m - 1 + m)) - m;;
let prefix_matches i p m = (mask i m = p)

let priority = function
    Empty -> failwith "PSQueue.priority"
  | Leaf (_, _, pri) -> pri
  | Branch (_, _, _, pri, _, _) -> pri
;;
  
let branch p m l r =
  let pri_l = priority l in
  let pri_r = priority r in
    if pri_l < pri_r
    then Branch (p, m, Left,  pri_l, l, r)
    else Branch (p, m, Right, pri_r, l, r)
;;
  
let join m p1 s1 p2 s2 =
  let m = branching_bit m p1 p2 in
  let p = mask p1 m
  in
    if p1 < p2
    then branch p m s1 s2
    else branch p m s2 s1
;;

let rec get s i = match s with
    Empty -> raise Not_found
  | Leaf (j, x, pri) when j = i -> x, pri
  | Leaf _ -> raise Not_found
  | Branch (p, _, _, _, l, r) ->
      if i <= p
      then get l i
      else get r i
;;

let rec top = function
    Empty -> raise Not_found
  | Leaf (i, x, pri) -> (i, x, pri)
  | Branch (_, _, Left,  _, l, _) -> top l
  | Branch (_, _, Right, _, _, r) -> top r
;;

let rec add s i x pri = match s with
    Empty -> Leaf (i, x, pri)
  | Leaf (j, _, _) when j = i -> Leaf (i, x, pri)
  | Leaf (j, _, _) -> join 1 i (Leaf (i, x, pri)) j s
  | Branch (p, m, _, _, l, r) ->
      if prefix_matches i p m then
	if i <= p
	then branch p m (add l i x pri) r
	else branch p m l (add r i x pri)
      else
	join (m lsl 1) i (Leaf (i, x, pri)) p s
;;

let rec remove s i =
  let fix_branch = function
    | (_, _, Empty, s) -> s
    | (_, _, s, Empty) -> s
    | (p, m, l, r) -> branch p m l r
  in
    match s with
	Empty -> Empty
      | Leaf (j, _, _) when j = i -> Empty
      | Leaf _ -> s
      | Branch (p, m, _, _, l, r) ->
	  if prefix_matches i p m then
	    if i <= p
	    then fix_branch (p, m, remove l i, r)
	    else fix_branch (p, m, l, remove r i)
	  else
	    s
;;

let pop s = let i, _, _ = top s in remove s i;;

let rec fold f s accu = match s with
    Empty -> accu
  | Leaf (i, x, pri) -> f accu i x pri
  | Branch (_, _, _, _, l, r) -> fold f r (fold f l accu)
;;
