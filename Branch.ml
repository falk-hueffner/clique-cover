(* Find an edge to branch on. ECC.UNCOVERED is nonempty.  *)
let spc n = String.make n ' ';;

let branch_calls = ref 0L;;

let rec branch ecc depth =
  branch_calls := Int64.succ !branch_calls;
  if ECC.all_covered ecc
  then Some (ECC.restore ecc [])
  else if ECC.k_used_up ecc
  then None
  else
    let i, j = ECC.branching_edge ecc in
    let neighbors =
      IntSet.intersection
	(Graph.neighbors (ECC.g ecc) i)
	(Graph.neighbors (ECC.g ecc) j) in
    let cliques =
      Cliques.max_cliques (Graph.subgraph (ECC.g ecc) neighbors) in
    let cliques = List.map (fun s -> IntSet.add s i) cliques in
    let cliques = List.map (fun s -> IntSet.add s j) cliques in
      Util.list_find_opt
	(fun clique ->
	   let ecc = ECC.cover ecc clique in
	   let ecc = ECC.reduce ecc in
	     branch ecc (depth + 1))
	cliques
;;

let ecc_solve g =
  let ecc = ECC.make g in
  let rec loop k =
    let ecc = ECC.set_max_k ecc k in
    let ecc = ECC.reduce ecc in
      if !Util.verbose then Printf.eprintf "*** k = %d ***\n%!" k;
      match branch ecc 0 with
          None -> loop (k + 1)
	| Some cliques -> cliques
  in
    loop (ECC.k ecc)
;;
