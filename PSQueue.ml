module M = Map.Make(struct type t = int let compare = compare end);;

type 'a t = ('a * int) M.t;;

exception Empty;;

let empty = M.empty;;

let add q k v p = M.add k (v, p) q;;

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

let get q k = M.find k q;;

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

let fold f q x = M.fold (fun k (v, p) x -> f x k v p) q x;;
