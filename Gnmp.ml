let gnmp n m p =  
  let features = Array.make n IntSet.empty in
  for v = 0 to n - 1 do
    for f = 0 to m - 1 do
      if Random.float 1.0 <= p
      then features.(v) <- IntSet.add features.(v) f
    done
  done;
  let g = ref Graph.empty in
  for v = 0 to n - 1 do
    for w = v + 1 to n - 1 do
      if IntSet.do_intersect features.(v) features.(w)
      then g := Graph.connect !g v w
    done
  done;
  features, !g
;;

let () =
  if Array.length Sys.argv < 4 || Array.length Sys.argv > 5
  then begin
    Printf.eprintf "usage: %s n m edges [rand-seed]\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  let m = int_of_string Sys.argv.(2) in
  let edges = int_of_string Sys.argv.(3) in

  if Array.length Sys.argv = 5
  then Random.init (int_of_string Sys.argv.(4))
  else Random.self_init ();
  let features, g, p =
    let rec loop p delta =
      let features, g = gnmp n m p in    
      let edges' = Graph.num_edges g in
	if delta < 1e-16 || (float_of_int (abs (edges' - edges))) /. (float_of_int edges) < 0.01
	then features, g, p
	else if edges' > edges
	then loop (p -. delta) (0.75 *. delta)
	else loop (p +. delta) (0.75 *. delta)
    in
      loop 0.5 0.25
  in
  Printf.printf "# n = %d\n# m = %d\n# p = %f\n" n m p;
  let features, g = gnmp n m p in
  for f = 0 to m - 1 do
    Printf.printf "# %d:" f;
    for v = 0 to n - 1 do
      if IntSet.contains features.(v) f
      then Printf.printf " %d" v
    done;
    print_newline ();
  done;

  Graph.iter_edges (fun i j -> Printf.printf "%d %d\n" i j) g
;;
