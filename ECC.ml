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

let make g =
  Printf.eprintf "heating up cache...%!";
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
    Printf.eprintf "done\n%!";
    { g         = g;
      uncovered = g;
      k         = 0;
      cache     = cache;
    }
;;

let set_k ecc k = { ecc with k = k }

let branching_edge ecc =
  let (i, j), _, score = PSQueue.top ecc.cache in
(*     Printf.eprintf "selecting edge %d %d with score %d\n%!" i j score; *)
    (i, j)
;;

let cover ecc clique =
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
  let g =
    IntSet.fold
      (fun g i ->
	 if Graph.is_deg0 ecc.uncovered i
	 then Graph.delete_vertex g i
	 else g)
      clique
      ecc.g in
    { g = g; uncovered = uncovered; k = ecc.k - 1; cache = cache }
;;
