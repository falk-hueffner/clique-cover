let sweep g cliques =
  let coverage = Array.make_matrix (Graph.num_vertices g) (Graph.num_vertices g) 0 in
  let update i j d =
    let i, j = if i < j then i, j else j, i in
      coverage.(i).(j) <- coverage.(i).(j) + d in
  let get i j =
    let i, j = if i < j then i, j else j, i in
      coverage.(i).(j) in    
  List.iter
    (fun clique ->
       IntSet.iter
	 (fun i -> IntSet.iter
	    (fun j ->
	       if i < j then update i j 1) clique) clique)
    cliques;
  let cliques =
    List.fold_left
      (fun cliques clique ->
	 let clique' =
	   IntSet.fold
	     (fun clique' i ->
		if IntSet.for_all (fun j -> i = j || get i j > 1) clique
		then begin
		  IntSet.iter (fun j -> if i <> j then update i j (-1)) clique;
		  clique';
		end else IntSet.add clique' i)	    
	     clique
	     IntSet.empty in
	   clique' :: cliques)
      []
      cliques
  in
    List.filter (fun clique -> IntSet.size clique >= 2) cliques
;;
