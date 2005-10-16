type t = IntSet.t IntMap.t;;

let empty = IntMap.empty;;

let make_clique k =
  let neigh = Util.fold_n IntSet.add k IntSet.empty in
    Util.fold_n (fun g i -> IntMap.add g i (IntSet.remove neigh i)) k empty
;;

let has_vertex g i = IntMap.has_key g i;;

let neighbors g i = IntMap.get g i;;
let neighbors' g i = IntMap.get_default g i IntSet.empty;;

let choose_edge g =
  let i, neighbors =
    IntMap.find (fun _ neighbors -> not (IntSet.is_empty neighbors)) g
  in
    i, IntSet.choose neighbors
;;

let add_vertex g i =
  if IntMap.has_key g i
  then g
  else IntMap.add g i IntSet.empty
;;

let new_vertex g = (IntMap.max_key g) + 1;;

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

let of_graph6 b =
  for i = 0 to String.length b - 1 do
    b.[i] <- char_of_int ((int_of_char b.[i]) - 63)
  done;
  let n = int_of_char b.[0] in
  let get_bit i =
    let byte = 1 + (i / 6) in
    let bit = 5 - (i mod 6) in
      (((int_of_char b.[byte]) lsr bit) land 1) <> 0 in
  let rec loop g i j bit =
    if i = j then
      if j < n - 1 then loop g 0 (j + 1) bit else g
    else
      let g = 
	if get_bit bit
	then connect g i j
	else g in
	loop g (i + 1) j (bit + 1)
  in
    loop empty 0 1 0
;;

let is_connected g i j = IntSet.contains (IntMap.get g i) j;;

let subgraph g vs =
  IntSet.fold
    (fun g' v ->
       IntMap.add g' v (IntSet.intersection (neighbors g v) vs))
    vs
    empty
;;

let fold_subgraph_edges f g vs accu =
  IntSet.fold
    (fun accu i ->
       IntSet.fold_intersection
	 (fun accu j -> if i < j then f accu i j else accu)
	 (neighbors g i)
	 vs
	 accu)
    vs
    accu
;;

let clear_subgraph g vs =
  IntSet.fold
    (fun g' v ->
       IntMap.add g' v (IntSet.minus (neighbors g v) vs))
    vs
    g
;;

let complete_subgraph g vs =
  IntSet.fold
    (fun g' v ->
       IntMap.add g' v (IntSet.union (neighbors g v) (IntSet.remove vs v)))
    vs
    g
;;

let num_edges_in_subgraph g vs =
  let num =
    IntSet.fold
      (fun num v ->
        num + (IntSet.intersection_size (neighbors g v) vs))
      vs
      0
  in
    assert (num mod 2 = 0);
    num / 2
;;

let fold_vertices f g x = IntMap.fold f g x;;
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
    IntMap.for_all
      (fun _ neighbors -> IntSet.size neighbors = d)
      g
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

let num_vertices = IntMap.size;;

let num_edges g =
  let num =
    fold_vertices
      (fun num _ neighbors -> num + IntSet.size neighbors) g 0
  in
    assert (num mod 2 = 0);
    num / 2
;;

let complement g =
  let vertices = vertices g
  in
    IntMap.map (fun i neighbors ->
		  IntSet.remove (IntSet.minus vertices neighbors) i) g
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
