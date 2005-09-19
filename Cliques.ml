let max_cliques g =
  (* returns the accumulator CLIQUES plus all maximal cliques C that
     are supersets of CLIQUE, disjoint from NOTS, and subset of CANDS
     union CLIQUE.  *)
  let rec extend clique cands nots cliques =
    if IntSet.is_empty cands && IntSet.is_empty nots
    then clique :: cliques
    else
      let rec loop cands nots cliques =
	if IntSet.is_empty cands
	then cliques
	else
	  let u = IntSet.choose cands in
	  let cands = IntSet.remove cands u in
	  let clique' = IntSet.add clique u in
	  let u_neighbors = Graph.neighbors g u in
	  let cands' = IntSet.intersection cands u_neighbors in
	  let nots' = IntSet.intersection nots u_neighbors in
	  let cliques = extend clique' cands' nots' cliques in
	  let nots = IntSet.add nots u in
	  loop cands nots cliques
      in
        loop cands nots cliques
  in
    extend IntSet.empty (Graph.vertices g) IntSet.empty []
;;
    













    (*
let find_max folder scorer collection =
  folder
    (fun ((best, best_score) as old) candidate ->
      let score = scorer candidate in
        if score > best_score
	then Some candidate, score
	else old)
    collection
    (None, min_int)
;;

    

let opt_default opt default =
  match opt with
    Some x -> x
  | None -> default
;;
    (* Look for nodes that are "candidates" with the
       most neighbors among the "candidates"  *)
    let best, _ =
      find_max
	IntSet.fold
	(fun cand -> IntSet.intersection_size (Graph.neighbors g cand) cands)
	cands in
    let best = opt_default best (IntSet.choose cands) in
    
*)
