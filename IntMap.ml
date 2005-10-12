type 'a t =
    Empty
  | Leaf of int * 'a
  | Branch of int * int * int * 'a t * 'a t
;;

let empty = Empty;;

let is_empty s = s = Empty;;

let size = function
    Empty -> 0
  | Leaf _ -> 1
  | Branch (_, _, c, _, _) -> c
;;

let rec has_key s i = match s with
    Empty -> false
  | Leaf (j, _) -> j = i
  | Branch (p, _, _, l, r) ->
      if i <= p
      then has_key l i
      else has_key r i
;;

let rec max_key s = match s with
    Empty -> raise Not_found
  | Leaf (i, _) -> i
  | Branch (_, _, _, _, r) -> max_key r
;;

let rec get s i = match s with
    Empty -> raise Not_found
  | Leaf (j, x) when j = i -> x
  | Leaf _ -> raise Not_found
  | Branch (p, _, _, l, r) ->
      if i <= p
      then get l i
      else get r i
;;

let rec get_default s i x = match s with
    Empty -> x
  | Leaf (j, x) when j = i -> x
  | Leaf _ -> x
  | Branch (p, _, _, l, r) ->
      if i <= p
      then get_default l i x
      else get_default r i x
;;

let rec highest_bit x =
  let x' = x land (x - 1) in
    if x' = 0
    then x
    else highest_bit x'
;;

let branching_bit m p1 p2 =
  let x = p1 lxor p2 in
  let x = x land lnot (m - 1) in
    highest_bit x;;

let mask i m = (i lor (m - 1 + m)) - m;;
let prefix_matches i p m = (mask i m = p)

let join m p1 s1 p2 s2 c =
  let m = branching_bit m p1 p2 in
  let p = mask p1 m
  in
    if p1 < p2
    then Branch (p, m, c, s1, s2)
    else Branch (p, m, c, s2, s1)
;;

let branch p m l r = Branch (p, m, size l + size r, l, r);;

let rec add s i x = match s with
    Empty -> Leaf (i, x)
  | Leaf (j, _) when j = i -> Leaf (i, x)
  | Leaf (j, _) -> join 1 i (Leaf (i, x)) j s 2
  | Branch (p, m, c, l, r) ->
      if prefix_matches i p m then
	if i <= p
	then branch p m (add l i x) r
	else branch p m l (add r i x)
      else
	join (m lsl 1) i (Leaf (i, x)) p s (c + 1)
;;

let rec remove s i =
  let branch = function
    | (_, _, Empty, s) -> s
    | (_, _, s, Empty) -> s
    | (p, m, l, r) -> Branch (p, m, size l + size r, l, r)
  in
    match s with
	Empty -> Empty
      | Leaf (j, _) when j = i -> Empty
      | Leaf _ -> s
      | Branch (p, m, c, l, r) ->
	  if prefix_matches i p m then
	    if i <= p
	    then branch (p, m, remove l i, r)
	    else branch (p, m, l, remove r i)
	  else
	    s
;;

let rec update f s i = match s with
    Empty -> raise Not_found
  | Leaf (j, x) when j = i -> Leaf (i, f x)
  | Leaf _ -> raise Not_found
  | Branch (p, m, c, l, r) ->
      if i <= p
      then Branch (p, m, c, update f l i, r)
      else Branch (p, m, c, l, update f r i)
;;

let rec fold f s accu = match s with
    Empty -> accu
  | Leaf (i, x) -> f accu i x
  | Branch (_, _, _, l, r) -> fold f r (fold f l accu)
;;

let rec iter f s = match s with
    Empty -> ()
  | Leaf (i, x) -> f i x
  | Branch (_, _, _, l, r) -> iter f l; iter f r
;;

let rec map f = function
    Empty -> Empty
  | Leaf (i, x) -> Leaf (i, f i x)
  | Branch (p, m, c, l, r) -> Branch (p, m, c, map f l, map f r)
    
let rec find_opt p = function
    Empty -> None
  | Leaf (i, x) -> p i x
  | Branch (_, _, _, l, r) ->
      match find_opt p l with
	None -> find_opt p r
      | some -> some
;;

let find p s =
  match find_opt (fun i x -> if p i x then Some (i, x) else None) s with
      None -> raise Not_found
    | Some x -> x
;;

let rec for_all p = function
    Empty -> true
  | Leaf (i, x) -> p i x
  | Branch (_, _, _, l, r) -> for_all p l && for_all p r
;;

let output printer channel s =
  Printf.fprintf channel "{[%d] " (size s);
  iter (fun i x  -> Printf.fprintf channel "%d: %a; " i printer x) s;
  Printf.fprintf channel "}";
;;

let print printer s = output printer stdout s;;
let dump  printer s = output printer stderr s;;
