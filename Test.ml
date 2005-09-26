let octahedron_graph t =
  let o = Graph.make_clique (2 * t) in
    Util.fold_n (fun o i -> Graph.disconnect o (2 * i) (2 * i + 1)) t o
;;

let graph_of_list =
  List.fold_left (fun g (i, j) -> Graph.connect g i j) Graph.empty
;;

let g_1 =
  graph_of_list [
    (0, 1);
    (0, 2);
    (0, 3);
    (1, 2);
    (1, 3);
    (1, 5);
    (1, 6);
    (2, 3);
    (2, 4);
    (2, 5);
    (3, 4);
    (3, 5);
    (4, 5);
    (5, 6);
  ]
;;

(*
let () =
  Util.print_list IntSet.output (Cliques.max_cliques g_1);
  print_newline();
  Util.print_list IntSet.output (Cliques.max_cliques (octahedron_graph 3));
  print_newline();
  let i = 20 in
    Printf.printf "O(%d) -> %d\n%!"
      i (List.length (Cliques.max_cliques (octahedron_graph i)));
    exit(0);
;;
*)
