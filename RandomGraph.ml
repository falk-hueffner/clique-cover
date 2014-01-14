let () =
  if Array.length Sys.argv < 3 || Array.length Sys.argv > 4
  then begin
    Printf.eprintf "usage: %s vertices edge-probability [seed]\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  if Array.length Sys.argv = 4
  then Random.init (int_of_string Sys.argv.(3))
  else Random.self_init ();
  let p = float_of_string Sys.argv.(2) in
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
