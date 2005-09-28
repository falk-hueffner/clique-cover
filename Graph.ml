type t = IntSet.t IntMap.t;;

let empty = IntMap.empty;;

let make_clique k =
  let neigh = Util.fold_n IntSet.add k IntSet.empty in
    Util.fold_n (fun g i -> IntMap.add g i (IntSet.remove neigh i)) k empty
;;

let has_vertex g i = IntMap.has_key g i;;

let neighbors g i = IntMap.get g i;;
let neighbors' g i = try neighbors g i with Not_found -> IntSet.empty;;

let choose_edge g =
  let i, neighbors =
    IntMap.find (fun _ neighbors -> not (IntSet.is_empty neighbors)) g
  in
    i, IntSet.choose neighbors
;;

let connect g i j =
  assert (i <> j);
  let neighbors_i = neighbors' g i in
  let neighbors_j = neighbors' g j in
  let neighbors_i = IntSet.add neighbors_i j in
  let neighbors_j = IntSet.add neighbors_j i in
  let g = IntMap.add g i neighbors_i in
  let g = IntMap.add g j neighbors_j in
    g
;;

let disconnect g i j =
  assert (i <> j);
  let neighbors_i = neighbors' g i in
  let neighbors_j = neighbors' g j in
  let neighbors_i = IntSet.remove neighbors_i j in
  let neighbors_j = IntSet.remove neighbors_j i in
  let g = IntMap.add g i neighbors_i in
  let g = IntMap.add g j neighbors_j in
    g
;;

let delete_vertex g i =
  let n = neighbors g i in
  let g = IntMap.remove g i in
    IntSet.fold
      (fun g j ->
	 let n' = IntSet.remove (neighbors g j) i in
	   IntMap.add g j n')
      n
      g
;;

let is_connected g i j = IntSet.contains (IntMap.get g i) j;;

let subgraph g vs =
  IntSet.fold
    (fun g' v ->
      IntMap.add g' v (IntSet.intersection (neighbors g v) vs))
    vs
    empty
;;

let clear_subgraph g vs =
  IntSet.fold
    (fun g' v ->
       IntMap.add g' v (IntSet.minus (neighbors g v) vs))
    vs
    g
;;

let fold_vertices f g x = IntMap.fold f g x;;
let fold_vertices_inorder f g x = IntMap.fold_inorder f g x;;
let iter_vertices f g = fold_vertices (fun () i neighbors -> f i neighbors) g ();;

let vertices g =
  fold_vertices
    (fun vertices i _ -> IntSet.add vertices i)
    g
    IntSet.empty
;;

let num_vertices = IntMap.size;;

let deg g v = IntSet.size (neighbors g v);;
let is_deg0 g v = IntSet.is_empty (neighbors g v);;

let is_clique g =
  let d = (num_vertices g) - 1 in
    fold_vertices
      (fun is_clique _ n -> is_clique && IntSet.size n = d)
      g
      true
;;

let fold_neighbors f g i accu = IntSet.fold f (neighbors g i) accu;;

let fold_edges f g accu =
  fold_vertices
    (fun accu i neighbors ->
       IntSet.fold
	 (fun accu j -> if i < j then f accu i j else accu)
	 neighbors
	 accu)
    g
    accu
;;

let iter_edges f g = fold_edges (fun () i j -> f i j) g ();;

let map = IntMap.map;;

let num_vertices g = fold_vertices (fun n _ _ -> n + 1) g 0;;
let num_edges g = fold_edges (fun n _ _ -> n + 1) g 0;;

let complement g =
  let vertices = vertices g
  in
    map (fun i neighbors ->
          IntSet.remove (IntSet.minus vertices neighbors) i) g
;;

let find_edge_opt p g =
  IntMap.find_opt
    (fun i neighbors ->
      IntSet.find_opt (fun j -> p i j) neighbors)
    g
;;

let output channel g =
  Printf.fprintf channel "{\n";
  iter_vertices (fun i neighbors ->
	           if IntSet.is_empty neighbors
		   then Printf.fprintf channel "%d\n" i) g;
  iter_edges (fun i j -> Printf.fprintf channel "%d %d\n" i j) g;
  Printf.fprintf channel "}\n";
;;

let print g = output stdout g;;
let dump g = output stderr g;;

