open ECC;;

(* Find an edge to branch on. ECC.UNCOVERED is nonempty.  *)
let find_branching_edge ecc =
  Graph.choose_edge ecc.uncovered;;
(*
  let i, j, _, _ =
    Graph.fold_edges
      (fun (best_i, best_j, best_c, best_a) i j ->
	 let neighbors =
	   IntSet.intersection (Graph.neighbors g i) (Graph.neighbors g j) in
	 let num_neigbors = IntSet.size neighbors in
	 let num_clique_edges = (num_neigbors * (num_neigbors - 1)) / 2 in
	 let num_actual_edges = Graph.num_edges (Graph.subgraph g neighbors) in
	   if best_i = -1
	     || num_clique_edges - num_actual_edges < best_c - best_a
	     || num_clique_edges - num_actual_edges = best_c - best_a
	     && num_clique_edges < best_c
	   then (i, j, num_clique_edges, num_actual_edges)
	   else (best_i, best_j, best_c, best_a))
      uncovered
      (-1, 0, 0, 0)
  in
    i, j
;;
*)

let spc n = String.make n ' ';;

let rec branch ecc depth =
  if Graph.num_edges ecc.uncovered = 0
  then Some []
  else if ecc.k <= 0
  then None
  else
    let i, j = find_branching_edge ecc in
(*     Printf.eprintf "%sbranch on %d %d%!" (spc depth) i j; *)
    let neighbors =
      IntSet.intersection (Graph.neighbors ecc.g i) (Graph.neighbors ecc.g j) in
    let cliques =
      Cliques.max_cliques (Graph.subgraph ecc.g neighbors) in
(*     Printf.eprintf " -> %a\n%!" (Util.output_list IntSet.output) cliques; *)
    let cliques = List.map (fun s -> IntSet.add s i) cliques in
    let cliques = List.map (fun s -> IntSet.add s j) cliques in
      Util.list_find_opt
	(fun clique ->	   
	   let uncovered = Graph.clear_subgraph ecc.uncovered clique in
	     match branch { g = ecc.g; uncovered = uncovered; k = ecc.k - 1 } (depth + 1) with
		 None -> None
	       | Some cover -> Some (clique :: cover))
	cliques
;;

let ecc_solve g k =
  branch { g = g; uncovered = g; k = k; } 0
;;
