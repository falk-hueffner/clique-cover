type t = {
  g:	     Graph.t;
  uncovered: Graph.t;
  k:	     int;
  (* Caches  *)
  cache:     (IntSet.t * int) PSQueue.t;
};;

let g ecc = ecc.g;;
let k ecc = ecc.k;;
let uncovered ecc = ecc.uncovered;;

let all_covered ecc = PSQueue.is_empty ecc.cache;;

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

(* Reduce vertices adjacent to no uncovered edge. Restrict search to
   VERTICES. *)
let reduce_deg0vertices ecc vertices =
  let g, uncovered =
    IntSet.fold
      (fun (g, uncovered) i ->
	 if Graph.is_deg0 uncovered i
	 then Graph.delete_vertex g i, Graph.delete_vertex uncovered i
	 else g, uncovered)
      vertices
      (ecc.g, ecc.uncovered) in
    (* FIXME update cache *)
    { ecc with g = g; uncovered = uncovered }
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
	 let neighbors =
	   IntSet.intersection
	     (Graph.neighbors g i)
	     (Graph.neighbors g j) in
	 let num_neigbors = IntSet.size neighbors in
	 let num_clique_edges = (num_neigbors * (num_neigbors - 1)) / 2 in
	 let num_actual_edges = Graph.num_edges (Graph.subgraph g neighbors) in
	 let score = num_clique_edges - num_actual_edges in
	   PSQueue.add cache (i, j) (neighbors, num_neigbors) score)
      g
      PSQueue.empty
  in
    if !Util.verbose then Printf.eprintf "done\n%!";
    let ecc = {
      g         = g;
      uncovered = g;
      k         = 0;
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
  let ecc = reduce_deg0vertices ecc clique in
    ecc, restorer
;;
