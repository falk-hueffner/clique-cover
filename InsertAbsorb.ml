let ecc_heuristic g =
  Graph.fold_edges
    (fun cliques i j ->
       let old_cliques, new_cliques =
	 List.fold_left
	   (fun (old_cliques, new_cliques) clique ->
	      if not (IntSet.contains clique i && IntSet.contains clique j)
	      then (clique :: old_cliques, new_cliques)
	      else
		let clique1 = IntSet.remove clique i in
		let clique2 = IntSet.remove clique j in
		  (old_cliques, clique1 :: clique2 :: new_cliques))
	   ([], [])
	   cliques in
       let new_cliques =
	 List.fold_left
	   (fun new_cliques clique ->
	      if List.exists
		(fun old_clique -> IntSet.is_subset clique old_clique)
		old_cliques
	      then new_cliques
	      else clique :: new_cliques)
	   []
	   new_cliques
       in
	 List.rev_append old_cliques new_cliques)
    (Graph.complement g)
    [ Graph.vertices g ]
;;
