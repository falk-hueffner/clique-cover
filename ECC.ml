type t = {
  g:	     Graph.t;
  uncovered: Graph.t;
  k:	     int;
  max_k:     int;
  (* Caches  *)
  cache:     IntSet.t PSQueue.t;
};;

let use_rule1        = ref true;;
let use_rule2        = ref true;;
let use_rule3        = ref true;;
let use_rule4        = ref true;;

let rule1_counter = ref (Int64.zero);;
let rule2_counter = ref (Int64.zero);;
let rule3_counter = ref (Int64.zero);;
let rule4_counter = ref (Int64.zero);;

let g ecc = ecc.g;;
let k ecc = ecc.k;;
let uncovered ecc = ecc.uncovered;;
let k_used_up ecc = ecc.k >= ecc.max_k;;

let all_covered ecc = PSQueue.is_empty ecc.cache;;

let set_max_k ecc max_k = { ecc with max_k = max_k };;

let identity x = x;;
let (@@) f g = fun x -> f (g x);;

let pack i j =
  assert (i >= 0 && i < 1 lsl 14);
  assert (j >= 0 && j < 1 lsl 14);
  let i, j = if i < j then i, j else j, i in
    i lor (j lsl 15)
;;
let unpack x =
  x land ((1 lsl 15) - 1), x lsr 15
;;

let do_cover ecc clique =
  assert (not (k_used_up ecc));
  let uncovered = Graph.clear_subgraph ecc.uncovered clique in
  let cache =
    IntSet.fold
      (fun cache i ->
	 IntSet.fold
	   (fun cache j ->	      
	      if i < j
	      then PSQueue.remove cache (pack i j)
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
  let num_actual_edges = Graph.num_edges_in_subgraph g neighbors in
  let score = num_clique_edges - num_actual_edges in
    neighbors, score
;;

let verify_cache ecc =
  PSQueue.fold
    (fun () edge neighbors score ->
       let i, j = unpack edge in
       let neighbors', score' = edge_score ecc.g i j in
	 if not (Graph.is_connected ecc.uncovered i j)
	 then Printf.eprintf "bogus edge %d %d\n%!" i j;
	 if not (IntSet.equal neighbors neighbors')
	 then Printf.eprintf "bogus neighbor set for %d %d: %a, should be %a\n%!"
	   i j IntSet.output neighbors IntSet.output neighbors';
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
    Graph.fold_neighbors
      (fun cache j -> PSQueue.remove cache (pack i j))
      ecc.uncovered
      i
      ecc.cache in
  let cache =
    Graph.fold_subgraph_edges
      (fun cache j k ->
	 let neighbors, score = PSQueue.get cache (pack j k) in
	 let neighbors' = IntSet.remove neighbors i in
	 let num_neigbors = IntSet.size neighbors in
	 let num_neigbors' = num_neigbors - 1 in
	 let score' = score - num_neigbors'
	   + IntSet.intersection_size neighbors_i neighbors' in
	   PSQueue.add cache (pack j k) neighbors' score')
      ecc.uncovered neighbors_i
      cache
  in
    { ecc with g = g; uncovered = uncovered; cache = cache }
;;
  
(* Reduce vertices adjacent to no uncovered edge. Restrict search to
   VERTICES. *)
let reduce_deg0vertices ecc vertices =
  if not !use_rule1 then ecc else
  IntSet.fold
    (fun ecc i ->
       if not (Graph.is_deg0 ecc.uncovered i)
       then ecc
       else begin
	 Util.int64_incr rule1_counter;
	 del_vertex ecc i
       end)
    vertices
    ecc
;;

let reduce_only1maxcliq ecc =
  if not !use_rule2 then ecc, identity else
  let rec loop ecc restorer =
    if k_used_up ecc || PSQueue.is_empty ecc.cache
    then ecc, restorer
    else
      let edge, neighbors, score = PSQueue.top ecc.cache in
      let i, j = unpack edge in
	if score > 0
	then ecc, restorer
	else begin
	  Util.int64_incr rule2_counter;
	  let clique = IntSet.add neighbors i in
	  let clique = IntSet.add clique j in
	  let ecc, restorer' = do_cover ecc clique in
	    loop ecc (restorer @@ restorer')
	end
  in
    loop ecc identity
;;

let rec prison_reduce ecc vertices =
  if not !use_rule3 then ecc else
  if k_used_up ecc || IntSet.is_empty vertices
  then ecc
  else
    let i = IntSet.choose vertices in
    let vertices = IntSet.remove vertices i in
    let neigh = Graph.neighbors ecc.uncovered i in
    let prisoners, exits =
      IntSet.fold
	(fun (prisoners, exits) j ->
	   if IntSet.is_subset (Graph.neighbors ecc.g j) neigh
	   then IntSet.add prisoners j, exits
	   else prisoners, IntSet.add exits j)
	neigh
	(IntSet.empty, IntSet.empty)
    in
      if not(IntSet.for_all
	       (fun x -> IntSet.do_intersect (Graph.neighbors ecc.g x) prisoners)
	       exits)
      then prison_reduce ecc vertices
      else begin
	Util.int64_incr rule3_counter;
	(* FIXME prepare restorer  *)
	prison_reduce (del_vertex ecc i) vertices
      end
;;

let rec aerate ecc vertices =
  if not !use_rule4 || k_used_up ecc || IntSet.is_empty vertices
  then ecc, identity
  else
    let i = IntSet.choose vertices in
    let i_neighbors = Graph.neighbors ecc.g i in
    let i_neighbors_uncovered = Graph.neighbors ecc.uncovered i in
    let vertices = IntSet.remove vertices i in
    let colors =
      Graph.fold_neighbors
	(fun colors j ->
	   if IntMap.has_key colors j 
	   then colors
	   else
	     let color =
	       if IntMap.is_empty colors then 0
	       else (IntMap.max_key colors) + 1 in
	     let rec paint colors k =
	       if IntMap.has_key colors k
	       then colors
	       else
		 let colors = IntMap.add colors k color in
		   IntSet.fold
                     (fun colors l -> paint colors l)
		     (IntSet.intersection (Graph.neighbors ecc.g k) i_neighbors)
		     colors
             in
               paint colors j)
	ecc.g
	i
	IntMap.empty in
    let num_colors = IntMap.max_key colors
    in
      if num_colors = 1
      then aerate ecc vertices
      else begin
	Util.int64_incr rule4_counter;
	let new_vertices_start = Graph.new_vertex ecc.g in
	let new_vertex color = new_vertices_start + color in
	let new_neighbor j = new_vertex (IntMap.get colors j) in
	let g = Graph.delete_vertex ecc.g i in
	let uncovered = Graph.delete_vertex ecc.uncovered i in
	let g =
	  IntSet.fold (fun g j -> Graph.connect g j (new_neighbor j)) i_neighbors g in
	let uncovered =
	  IntSet.fold (fun g j -> Graph.connect g j (new_neighbor j)) i_neighbors uncovered in
	let cache =
	  IntSet.fold
	    (fun cache j ->
	       let neighbors, prio = PSQueue.get cache (pack j i) in
	       let cache = PSQueue.remove cache (pack j i) in
		 PSQueue.add cache (pack j (new_neighbor j)) neighbors prio)
	    i_neighbors_uncovered
	    ecc.cache in
	let cache =
	  Graph.fold_subgraph_edges
	    (fun cache j k ->
	       let neighbors, prio = PSQueue.get cache (pack j k) in
	       let neighbors = IntSet.remove neighbors i in
	       let neighbors = IntSet.add    neighbors (new_neighbor j) in		 
	       let cache = PSQueue.remove cache (pack j k) in
		 PSQueue.add cache (pack j k) neighbors prio)
	    ecc.uncovered
	    i_neighbors
	    cache in
	let restorer =
	  List.map
	    (fun clique ->
	       (IntSet.fold
		  (fun s j ->
		     let j = if Graph.has_vertex ecc.g j then j else i in
		       IntSet.add s j)
		  clique
		  IntSet.empty)) in		 
	let ecc = { ecc with g = g; uncovered = uncovered; cache = cache }
	in
	  verify_cache ecc;
	  ecc, restorer
      end

let make g =
  if !Util.verbose then Printf.eprintf "heating up cache...%!";
  let cache =
    Graph.fold_edges
      (fun cache i j ->
	 assert (i < j);
	 let neighbors, score = edge_score g i j in
	   PSQueue.add cache (pack i j) neighbors score)
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
(*     let ecc = prison_reduce ecc (Graph.vertices ecc.g) in *)
(*       Printf.printf "reduced to k = %d\n" ecc.k; *)
      ecc, restorer
;;

let branching_edge ecc =
  let edge, _, score = PSQueue.top ecc.cache in
    (*     Printf.eprintf "selecting edge %d %d with score %d\n%!" i j score; *)
    unpack edge
;;

let cover ecc clique =
  let ecc, restorer = do_cover ecc clique in
  let ecc, restorer' = reduce_only1maxcliq ecc in
  let restorer = restorer @@ restorer' in
  let ecc = reduce_deg0vertices ecc clique in
  let ecc = prison_reduce ecc (Graph.vertices ecc.g) in
  let ecc, restorer' = aerate ecc (Graph.vertices ecc.g) in
  let restorer = restorer @@ restorer' in
(*     verify_cache ecc; *)
    ecc, restorer
;;
