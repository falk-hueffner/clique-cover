(* Find an edge to branch on. ECC.UNCOVERED is nonempty.  *)
let spc n = String.make n ' ';;

(*
let reduce_singletons ecc =
  let g, uncovered =
    Graph.fold_vertices
      (fun (g, uncovered) i neighbors ->
	 if IntSet.is_empty neighbors
	 then Graph.delete_vertex g i, Graph.delete_vertex uncovered i
	 else g, uncovered)     
      ecc.uncovered
      (ecc.g, ecc.uncovered)      
  in
    { g = g; uncovered = uncovered; k = ecc.k }
;;
*)

let branch_calls = ref 0L;;

let rec branch ecc depth =
  branch_calls := Int64.succ !branch_calls;
  if ECC.all_covered ecc
  then Some []
  else if ECC.k ecc <= 0
  then None
  else
(*     let ecc = reduce_singletons ecc in *)
    let i, j = ECC.branching_edge ecc in
(*      Printf.eprintf "%sbranch on %d %d%!" (spc depth) i j;  *)
    let neighbors =
      IntSet.intersection (Graph.neighbors (ECC.g ecc) i) (Graph.neighbors (ECC.g ecc) j) in
    let cliques =
      Cliques.max_cliques (Graph.subgraph (ECC.g ecc) neighbors) in
    let cliques = List.map (fun s -> IntSet.add s i) cliques in
    let cliques = List.map (fun s -> IntSet.add s j) cliques in
(*     Printf.eprintf " -> %a\n%!" (Util.output_list IntSet.output) cliques; *)
      Util.list_find_opt
	(fun clique ->
	   match branch (ECC.cover ecc clique) (depth + 1) with
	       None -> None
	     | Some cover -> Some (clique :: cover))
	cliques
;;

let ecc_solve g k =
  branch (ECC.make g k) 0
;;
