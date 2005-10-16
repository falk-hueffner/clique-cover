type t = {
  g:	      Graph.t;
  uncovered:  Graph.t;
  k:	      int;
  max_k:      int;
  cache:      IntSet.t PSQueue.t;
  restorer:   IntSet.t list -> IntSet.t list;
  rule1_cand: IntSet.t;
  rule3_cand: IntSet.t;
  rule4_cand: IntSet.t;  
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
let restore ecc cliques = ecc.restorer cliques;;

let all_covered ecc = PSQueue.is_empty ecc.cache;;

let set_max_k ecc max_k = { ecc with max_k = max_k };;

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

let cover ecc clique =
  assert (not (k_used_up ecc));
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
      ecc.cache
  in
    { ecc with
	uncovered = Graph.clear_subgraph ecc.uncovered clique;
	k = ecc.k + 1;
	cache = cache;
	restorer = ecc.restorer @@ (fun cliques -> clique :: cliques);
	rule1_cand = clique }
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
let reduce_rule1 ecc =
  if not !use_rule1 then ecc else
  IntSet.fold
    (fun ecc i ->
       if not (Graph.is_deg0 ecc.uncovered i)
       then ecc
       else begin
	 Util.int64_incr rule1_counter;
	 del_vertex ecc i
       end)
    ecc.rule1_cand
    ecc
;;

let reduce_rule2 ecc =
  if not !use_rule2 then false, ecc else
  let rec loop did_reduce ecc =
    if k_used_up ecc || PSQueue.is_empty ecc.cache
    then did_reduce, ecc
    else
      let edge, neighbors, score = PSQueue.top ecc.cache in
      let i, j = unpack edge in
	if score > 0
	then did_reduce, ecc
	else begin
	  Util.int64_incr rule2_counter;
	  let clique = IntSet.add neighbors i in
	  let clique = IntSet.add clique j in
	    loop did_reduce (cover ecc clique)
	end
  in
    loop false ecc
;;

let rec reduce_rule3 ecc =
  if not !use_rule3 || k_used_up ecc || IntSet.is_empty ecc.rule3_cand
  then ecc
  else
    let i, rule3_cand = IntSet.pop ecc.rule3_cand in
    let ecc = { ecc with rule3_cand = rule3_cand } in
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
      then reduce_rule3 ecc
      else begin
	Util.int64_incr rule3_counter;
	(* FIXME prepare restorer  *)
	reduce_rule3 (del_vertex ecc i)
      end
;;

let rec reduce_rule4 ecc =
  if not !use_rule4 || k_used_up ecc || IntSet.is_empty ecc.rule4_cand
  then ecc
  else
    let i, rule4_cand = IntSet.pop ecc.rule4_cand in
    let ecc = { ecc with rule4_cand = rule4_cand } in
    let i_neighbors = Graph.neighbors ecc.g i in
    let i_neighbors_uncovered = Graph.neighbors ecc.uncovered i in
    let colors, num_colors =
      Graph.fold_neighbors
	(fun (colors, num_colors) j ->
	   if IntMap.has_key colors j 
	   then colors, num_colors
	   else
	     let color = num_colors in
	     let rec paint colors k =
	       if IntMap.has_key colors k
	       then colors
	       else
		 let colors = IntMap.add colors k color in
		   IntSet.fold		(* fold_intersection *)
                     (fun colors l -> paint colors l)
		     (IntSet.intersection (Graph.neighbors ecc.g k) i_neighbors)
		     colors
             in
               paint colors j, num_colors + 1)
	ecc.g
	i
	(IntMap.empty, 0)
    in
      if num_colors <= 1
      then reduce_rule4 ecc
      else begin
(*	Printf.printf "aerate %d %a %a %a\n"
	  i
	  IntSet.output i_neighbors
	  Graph.output (Graph.subgraph ecc.g  i_neighbors)
	  (IntMap.output Util.output_int) colors; *)
	Util.int64_incr rule4_counter;
	let new_vertices_start = Graph.new_vertex ecc.g in
	let new_vertex color = new_vertices_start + color in
	let new_neighbor j = new_vertex (IntMap.get colors j) in
	let g = Graph.delete_vertex ecc.g i in
	let uncovered = Graph.delete_vertex ecc.uncovered i in
	let g =
	  IntSet.fold (fun g j -> Graph.connect g j (new_neighbor j)) i_neighbors g in
	let uncovered =
	  Util.fold_n (fun g j -> Graph.add_vertex g (new_vertex j)) num_colors uncovered in
	let uncovered =
	  IntSet.fold (fun g j -> Graph.connect g j (new_neighbor j)) i_neighbors_uncovered uncovered in
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
	{ ecc with g = g; uncovered = uncovered; cache = cache;
	    restorer = ecc.restorer @@ restorer }
      end
;;

let rec reduce ecc =
  let _, ecc = reduce_rule2 ecc in
  let ecc = reduce_rule1 ecc in
  let ecc = { ecc with rule3_cand = Graph.vertices ecc.g } in    
  let ecc = reduce_rule3 ecc in
  let ecc = { ecc with rule4_cand = Graph.vertices ecc.g } in    
  let ecc = reduce_rule4 ecc in
(*     verify_cache ecc; *)
    ecc
;;

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
    let vertices = Graph.vertices g in
    let ecc = {
      g          = g;
      uncovered  = g;
      k          = 0;
      max_k      = 0;
      cache      = cache;
      restorer   = (fun cliques -> cliques);
      rule1_cand = vertices;
      rule3_cand = vertices;
      rule4_cand = vertices; } in
    let _, ecc = reduce_rule2 ecc in
    let ecc = reduce_rule1 ecc in
(*     let ecc = reduce_rule3 ecc (Graph.vertices ecc.g) in *)
(*       Printf.printf "reduced to k = %d\n" ecc.k; *)
      ecc
;;

let branching_edge ecc =
  let edge, _, score = PSQueue.top ecc.cache in
    (*     Printf.eprintf "selecting edge %d %d with score %d\n%!" i j score; *)
    unpack edge
;;
