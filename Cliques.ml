let max_cliques g =
  (* returns the accumulator CLIQUES plus all maximal cliques C that
     are supersets of CLIQUE, disjoint from NOTS, and subset of CANDS
     union CLIQUE.  *)
  let rec extend clique cands nots cliques =
    if IntSet.is_empty cands
    then
      if IntSet.is_empty nots
      then clique :: cliques
      else cliques
    else
      let pivot = IntSet.choose cands in
      let _, _, cliques =
	IntSet.fold
	  (fun ((cands, nots, cliques) as state) u ->
	     if Graph.is_connected g u pivot
	     then state
	     else
	       let cands = IntSet.remove cands u in
	       let clique' = IntSet.add clique u in
	       let u_neighbors = Graph.neighbors g u in
	       let cands' = IntSet.intersection cands u_neighbors in
	       let nots' = IntSet.intersection nots u_neighbors in
	       let cliques = extend clique' cands' nots' cliques in
	       let nots = IntSet.add nots u in
		 (cands, nots, cliques))
	  cands
	  (cands, nots, cliques)
      in
        cliques
  in
    extend IntSet.empty (Graph.vertices g) IntSet.empty []
;;
