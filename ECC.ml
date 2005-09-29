type t = {
  g:	     Graph.t;
  uncovered: Graph.t;
  k:	     int;
  max_k:     int;
  (* Caches  *)
  cache:     (IntSet.t * int) PSQueue.t;
};;

let g ecc = ecc.g;;
let k ecc = ecc.k;;
let uncovered ecc = ecc.uncovered;;
let k_used_up ecc = ecc.k >= ecc.max_k;;

let all_covered ecc = PSQueue.is_empty ecc.cache;;

let set_max_k ecc max_k = { ecc with max_k = max_k };;

let identity x = x;;

let (@@) f g = fun x -> f (g x);;

let do_cover ecc clique =
  let uncovered = Graph.clear_subgraph ecc.uncovered clique in
  let cache =
    IntSet.fold
      (fun cache i ->
	 IntSet.fold
	   (fun cache j ->	      
	      if i < j
	      then PSQueue.remove cache (i, j)
	      else cache)
	   clique
	   cache)
      clique
      ecc.cache in
  let ecc =
    { ecc with uncovered = uncovered; k = ecc.k + 1; cache = cache }
  in
    ecc, (fun cliques -> clique :: cliques)
;;

let edge_score g i j =
  let neighbors =
    IntSet.intersection
      (Graph.neighbors g i)
      (Graph.neighbors g j) in
  let num_neigbors = IntSet.size neighbors in
  let num_clique_edges = (num_neigbors * (num_neigbors - 1)) / 2 in
  let num_actual_edges = Graph.num_edges (Graph.subgraph g neighbors) in
  let score = num_clique_edges - num_actual_edges in
    neighbors, num_neigbors, score
;;

let verify_cache ecc =
  PSQueue.fold
    (fun () (i, j) (neighbors, num_neigbors) score ->
       let neighbors', num_neigbors', score' = edge_score ecc.g i j in
	 if not (Graph.is_connected ecc.uncovered i j)
	 then Printf.eprintf "bogus edge %d %d\n%!" i j;
	 if not (IntSet.equal neighbors neighbors')
	 then Printf.eprintf "bogus neighbor set for %d %d: %a, should be %a\n%!"
	   i j IntSet.output neighbors IntSet.output neighbors';
	 if num_neigbors <>  num_neigbors'
	 then Printf.eprintf "bogus neighbor set size for %d %d\n%!" i j;
	 if score <> score'
	 then Printf.eprintf "bogus score for %d %d\n%!" i j)
    ecc.cache
    ()
;;

let del_vertex ecc i =
  let g = Graph.delete_vertex ecc.g i in
  let uncovered = Graph.delete_vertex ecc.uncovered i in
  let neighbors_i = Graph.neighbors ecc.g i in
  let cache =
    Graph.fold_edges
      (fun cache j k ->
	 let (neighbors, num_neigbors), score = PSQueue.get cache (j, k) in
	 let neighbors' = IntSet.remove neighbors i in
	 let num_neigbors' = num_neigbors - 1 in
	 let score' = score - num_neigbors'
	   + IntSet.intersection_size neighbors_i neighbors' in
	   PSQueue.add cache (j, k) (neighbors', num_neigbors') score')
      (Graph.subgraph ecc.uncovered neighbors_i)
      ecc.cache
  in
    { ecc with g = g; uncovered = uncovered; cache = cache }
;;
  
(* Reduce vertices adjacent to no uncovered edge. Restrict search to
   VERTICES. *)
let reduce_deg0vertices ecc vertices =
  IntSet.fold
    (fun ecc i ->
       if not (Graph.is_deg0 ecc.uncovered i)
       then ecc
       else del_vertex ecc i)
    vertices
    ecc
;;

let reduce_only1maxcliq ecc =
  let rec loop ecc restorer =
    if PSQueue.is_empty ecc.cache
    then ecc, restorer
    else
      let (i, j), (neighbors, num_neigbors), score = PSQueue.top ecc.cache in
	if score > 0
	then ecc, restorer
	else
	  let clique = IntSet.add neighbors i in
	  let clique = IntSet.add clique j in
	  let ecc, restorer' = do_cover ecc clique in
	    loop ecc (restorer @@ restorer')
  in
    loop ecc identity
;;

let make g =
  if !Util.verbose then Printf.eprintf "heating up cache...%!";
  let cache =
    Graph.fold_edges
      (fun cache i j ->
	 assert (i < j);
	 let neighbors, num_neigbors, score = edge_score g i j in
	   PSQueue.add cache (i, j) (neighbors, num_neigbors) score)
      g
      PSQueue.empty
  in
    if !Util.verbose then Printf.eprintf "done\n%!";
    let ecc = {
      g         = g;
      uncovered = g;
      k         = 0;
      max_k     = 0;
      cache     = cache; } in
    let ecc, restorer = reduce_only1maxcliq ecc in
    let ecc = reduce_deg0vertices ecc (Graph.vertices ecc.g) in
(*       Printf.printf "reduced to k = %d\n" ecc.k; *)
      ecc, restorer
;;

let branching_edge ecc =
  let (i, j), _, score = PSQueue.top ecc.cache in
    (*     Printf.eprintf "selecting edge %d %d with score %d\n%!" i j score; *)
    (i, j)
;;

let cover ecc clique =
  let ecc, restorer = do_cover ecc clique in
  let ecc, restorer' = reduce_only1maxcliq ecc in
  let restorer = restorer @@ restorer' in
  let ecc = reduce_deg0vertices ecc clique in
(*     verify_cache ecc; *)
    ecc, restorer
;;
