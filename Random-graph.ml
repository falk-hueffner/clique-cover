let array_shuffle a =  
  let n = Array.length a in
  let swap i j =
    let tmp = a.(i) in
      a.(i) <- a.(j); a.(j) <- tmp
  in
    for i = 0 to n - 1 do
      swap i (i + (Random.int (n - i)))
    done
;;

let random_draw s k =
  let n = IntSet.size s in
  let a = Array.make n 0 in
    ignore (IntSet.fold (fun i x -> a.(i) <- x; i + 1) s 0);
    array_shuffle a;
    let rec loop s i =
      if i >= k then s
      else loop (IntSet.add s a.(i)) (i + 1)
    in
      loop IntSet.empty 0
;;

(*
let cliquegraph n k c =
  let g, cliques = 
    let v = Util.fold_n (fun v i -> IntSet.add v i) n IntSet.empty in    
    let g = Util.fold_n (fun g i -> Graph.add_vertex g i) n Graph.empty in
    let rec loop g cliques k =
      if k <= 0 then g, cliques
      else
	let v' = random_draw v c in
	  loop (Graph.complete_subgraph g v') (v' :: cliques) (k - 1)
    in
      loop g [] k
  in
    Printf.printf "# n = %d\n# k = %d\n# c = %d\n\n" n k c;
    List.iter
      (fun clique ->
         print_char '#';
         IntSet.iter (fun i -> Printf.printf " %d" i) clique;
         print_newline ())
      cliques;
    Graph.iter_edges
      (fun i j -> Printf.printf "%d %d\n" i j)
      g;
;;
*)

let cliquegraph n m c =
  let g, cliques = 
    let v = Util.fold_n (fun v i -> IntSet.add v i) n IntSet.empty in    
    let g = Util.fold_n (fun g i -> Graph.add_vertex g i) n Graph.empty in
    let rec loop g cliques =
      if Graph.num_edges g >= m  then g, cliques
      else
	let v' = random_draw v (2 + Random.int (c - 2)) in
	let g' = Graph.complete_subgraph g v' in
	  if Graph.num_edges g' >= (m * 101) / 100
	  then loop g cliques
	  else loop g' (v' :: cliques)
    in
      loop g []
  in
    Printf.printf "# n = %d\n# m = %d\n\n\n" n m;
    List.iter
      (fun clique ->
         print_char '#';
         IntSet.iter (fun i -> Printf.printf " %d" i) clique;
         print_newline ())
      cliques;
    Graph.iter_edges
      (fun i j -> Printf.printf "%d %d\n" i j)
      g;
;;

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 4
  then begin
    Printf.eprintf "usage: %s vertices                 \n" Sys.argv.(0);
    Printf.eprintf "       %s vertices edge-probability\n" Sys.argv.(0);
    Printf.eprintf "       %s vertices cliques clique-size\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  Random.self_init ();
  if Array.length Sys.argv = 4
  then cliquegraph n (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3))
  else
  let p =
    if  Array.length Sys.argv = 3
    then float_of_string Sys.argv.(2)
    else
      let n = float_of_int n in
	(n *. log n) /. ((n *. (n -. 1.0) /. 2.0))
  in
  let g =
    Util.fold_n
      (fun g i ->
	 Util.fold_n
	   (fun g j ->
	      if i < j && Random.float 1.0 < p
	      then Graph.connect g i j
	      else g)
	   n
	   g)
      n
      Graph.empty in
    Printf.printf "# n = %d\n# p = %f\n" n p;
    Graph.iter_edges
      (fun i j -> Printf.printf "%d %d\n" i j)
      g;
;;
