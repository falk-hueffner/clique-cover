module M = Map.Make(struct type t = int * int let compare = compare end);;

type 'a t = ('a * int) M.t;;

type key = int * int;;

exception Empty;;

let empty = M.empty;;

let add (q:'a t) k v (p:int) = M.add k (v, p) q;;

let is_empty q = M.is_empty q;;

let top q =
  match
    M.fold
      (fun k (v, p) best ->
	 match best with
	     Some (_, _, p') when p < p' -> Some (k, v, p)
	   | None                        -> Some (k, v, p)
	   | _                           -> best)
      q
      None
  with
      None -> raise Empty
    | Some ((k, v, p) as r) -> r
;;

let pop q =
  match
    M.fold
      (fun k (v, p) best ->
	 match best with
	     Some (_, p') when p < p' -> Some (k, p)
	   | None                     -> Some (k, p)
	   | _                        -> best)
      q
      None
  with
      None -> raise Empty
    | Some (k, _) -> M.remove k q
;;

let remove q k = M.remove k q;;
