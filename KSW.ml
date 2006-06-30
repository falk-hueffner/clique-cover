let ecc_heuristic g =
  let cliques = 
    Graph.fold_vertices
      (fun cliques i i_neighbors ->
	 let (w, present, _) = IntSet.split i_neighbors i in
	   assert (not present);
	   if IntSet.is_empty w
	   then
	     (IntSet.singleton i) :: cliques
	   else
	     (* Try to add w to each existing clique.  *)
	     let w, cliques =
	       let rec try_to_add w' cliques = function
		 | clique :: rest when not (IntSet.is_empty w') ->
		     if IntSet.is_subset clique w
		     then
		       let w' = IntSet.minus w' clique in
		       let clique = IntSet.add clique i in
			 try_to_add w' (clique :: cliques) rest
		     else
		       try_to_add w' (clique :: cliques) rest
		 | rest -> w', List.rev_append cliques rest
	       in
		 try_to_add w [] cliques in
	       (* For the still uncovered edges...  *)
	     let rec cover_rest w new_cliques =
	       if IntSet.is_empty w
	       then List.rev_append cliques new_cliques
	       else
		 let best, cliques =
		   (* ...try to cover as many at a time as possible.  *)
		   let rec find_best best best_inter_size others = function
		       [] -> best, others
		     | clique :: cliques ->
			 let inter_size = IntSet.intersection_size w clique in
			   if inter_size > best_inter_size
			   then find_best clique inter_size (best :: others) cliques
			   else find_best best best_inter_size (clique :: others) cliques
		   in
		     find_best IntSet.empty min_int [] cliques in
		 let new_clique = IntSet.intersection w best in
		 let new_clique = IntSet.add new_clique i in
		 let w = IntSet.minus w new_clique in
		   cover_rest w (new_clique :: new_cliques)
	     in
	       cover_rest w [])
      g
      [] in
    cliques
;;
