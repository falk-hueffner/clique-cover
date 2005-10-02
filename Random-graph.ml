let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3
  then begin
    Printf.eprintf "usage: %s vertices edge-probability\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  let p =
    if  Array.length Sys.argv = 3
    then float_of_string Sys.argv.(2)
    else
      let n = float_of_int n in
	(n *. log n) /. ((n *. (n -. 1.0) /. 2.0))
  in
  Random.self_init ();
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
