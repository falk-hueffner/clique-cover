(* Find an edge to branch on. ECC.UNCOVERED is nonempty.  *)
let spc n = String.make n ' ';;

let branch_calls = ref 0L;;

let rec branch ecc max_k depth =
  branch_calls := Int64.succ !branch_calls;
  if ECC.all_covered ecc
  then Some []
  else if ECC.k ecc >= max_k
  then None
  else
(*     let ecc = reduce_singletons ecc in *)
    let i, j = ECC.branching_edge ecc in
(*      Printf.eprintf "%sbranch on %d %d%!" (spc depth) i j;  *)
    let neighbors =
      IntSet.intersection
	(Graph.neighbors (ECC.g ecc) i)
	(Graph.neighbors (ECC.g ecc) j) in
    let cliques =
      Cliques.max_cliques (Graph.subgraph (ECC.g ecc) neighbors) in
    let cliques = List.map (fun s -> IntSet.add s i) cliques in
    let cliques = List.map (fun s -> IntSet.add s j) cliques in
(*     Printf.eprintf " -> %a\n%!" (Util.output_list IntSet.output) cliques; *)
      Util.list_find_opt
	(fun clique ->
	   match branch (ECC.cover ecc clique) max_k (depth + 1)  with
	       None -> None
	     | Some cover -> Some (clique :: cover))
	cliques
;;

let ecc_solve g =
  let ecc = ECC.make g in
  let rec loop k =
    if !Util.verbose then Printf.eprintf "*** k = %d ***\n%!" k;
    match branch ecc k 0 with
        None -> loop (k + 1)
      | Some cliques -> cliques
  in
    loop 0
;;
