let random_set n k =
  assert (n >= 0);
  assert (k > 0);
  assert (k >= n);
  let rec loop set =
    if IntSet.size set = n
    then set
    else loop (IntSet.add set (Random.int k))
  in
    loop IntSet.empty
;;
  
let () =
  if Array.length Sys.argv < 4 || Array.length Sys.argv > 5
  then begin
    Printf.eprintf "usage: %s n m s [rand-seed]\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  let m = int_of_string Sys.argv.(2) in
  if m > (n * (n - 1)) / 2 then failwith "m too large";
  let s = int_of_string Sys.argv.(3) in
  if Array.length Sys.argv = 5
  then Random.init (int_of_string Sys.argv.(4))
  else Random.self_init ();
  let m' = int_of_float (1.01 *. (float_of_int m)) in
  Printf.printf "# n = %d\n# m = %d\n# s = %d\n" n m s;
  let g =
    let rec loop g =
      if Graph.num_edges g >= m
      then g
      else
	let size = 2 + Random.int (s - 1) in
	let clique = random_set size n in
	let g' = Graph.complete_subgraph g clique in
	  if Graph.num_edges g' <= m'
	  then begin
	    print_char '#';
	    IntSet.iter (fun i -> Printf.printf " %d" i) clique;
	    print_newline ();
	    loop g'
	  end
	  else loop g
    in
      loop (Util.fold_n (fun g i -> Graph.add_vertex g i) n Graph.empty)
  in
    Graph.iter_edges (fun i j -> Printf.printf "%d %d\n" i j) g;
;;
