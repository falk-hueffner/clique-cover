type vertex = int;;
type neighborhood = IntSet.t;;
type t = neighborhood IntMap.t;;

let empty = IntMap.empty;;

let neighbors g i = IntMap.get g i;;
let neighbors' g i = try neighbors g i with Not_found -> IntSet.empty;;

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

let subgraph g vs =
  IntSet.fold
    (fun g' v ->
      IntMap.add g' v (IntSet.intersection (neighbors g v) vs))
    vs
    empty
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

let complement g =
  let vertices = vertices g
  in
    map (fun i neighbors ->
          IntSet.remove (IntSet.minus vertices neighbors) i) g
;;

let find_edge p g =
  IntMap.find
    (fun i neighbors ->
      IntSet.find (fun j -> p i j) neighbors)
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

