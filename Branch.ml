let ecc_branch g =
  let uncovered = g in
  let rec loop g uncovered cover =
    let (i, j, best_c, best_a) =
      Graph.fold_edges
	(fun (best_i, best_j, best_c, best_a) i j ->
	   let neighbors =
	     IntSet.intersection (Graph.neighbors g i) (Graph.neighbors g j) in
	   let num_neigbors = IntSet.size neighbors in
	   let num_clique_edges = (num_neigbors * (num_neigbors - 1)) / 2 in
	   let num_actual_edges = Graph.num_edges (Graph.subgraph g neighbors) in
(* 	     Printf.fprintf stderr "%d %d:  %d/%d\n%!" *)
(* 	       i j (*IntSet.output neighbors*) num_actual_edges num_clique_edges; *)
	     if best_i = -1
	       || num_clique_edges - num_actual_edges < best_c - best_a
	       || num_clique_edges - num_actual_edges = best_c - best_a
	         && num_clique_edges < best_c
	     then (i, j, num_clique_edges, num_actual_edges)
	     else (best_i, best_j, best_c, best_a))
	uncovered
	(-1, 0, 0, 0)
    in
      if i = -1
      then begin
	Printf.fprintf stderr "Everything's covered\n%!";
	cover
      end else begin
	Printf.fprintf stderr "branch on %d %d%!" i j;
	let neighbors =
	  IntSet.intersection (Graph.neighbors g i) (Graph.neighbors g j) in
	let cliques =
	  if best_a = best_c
	  then [ neighbors ]
	  else Cliques.max_cliques (Graph.subgraph g neighbors) in
	let cliques = List.map (fun s -> IntSet.add s i) cliques in
	let cliques = List.map (fun s -> IntSet.add s j) cliques in
 	Printf.fprintf stderr " -> %a\n%!" (Util.output_list IntSet.output) cliques;
	Printf.fprintf stderr " -> %d cliques\n%!" (List.length cliques);
	let _, cover =
	  List.fold_left
	    (fun (first, best_cover) clique ->
	       let uncovered = Graph.clear_subgraph uncovered clique in
	       let cover' = loop g uncovered (clique :: cover) in
		 if first || List.length cover' < List.length best_cover
		 then (false, cover')
		 else (false, best_cover))
	    (true, [])
	    cliques
	in
	  cover
      end	
  in
    loop g uncovered []
;;
	
