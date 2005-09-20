let isspace c = c = ' ' || c = '\t' || c = '\n' || c = '\r';;

let rec trim s =
  let len = String.length s in
    if s = ""
    then s
    else if isspace s.[0]
    then trim (String.sub s 1 (pred len))
    else if isspace s.[len - 1]
    then trim (String.sub s 0 (pred len))
    else s
;;

let rec explode s =
  if String.length s = 0
  then []
  else s.[0] :: (explode (String.sub s 1 (pred (String.length s))))
;;

let rec implode l =
  match l with
      [] -> ""
    | c :: cs -> (String.make 1 c) ^ (implode cs)
;;

let rec split_list s =
  match s with
      [] -> []
    | c :: cs ->
        if isspace c
        then split_list cs
        else
          let rec loop cs word =
            match cs with
                [] -> [List.rev word]
              | c :: cs ->
                  if isspace c
                  then (List.rev word) :: (split_list cs)
                  else loop cs (c :: word)
          in loop cs [c]
;;

let split_line l = List.map implode (split_list (explode l));;

let read_graph () =
  let vertex_numbers = Hashtbl.create 101 in
  let vertex_names   = Hashtbl.create 101 in
  let next_vertex = ref 0 in
  let vertex_number name =
    if not (Hashtbl.mem vertex_numbers name)
    then begin
      Hashtbl.replace vertex_numbers name !next_vertex;
      Hashtbl.replace vertex_names   !next_vertex name;
      incr next_vertex;      
    end;
    Hashtbl.find vertex_numbers name in
  let edges =
    let rec loop l =
      try
        let line = trim (read_line ()) in
          (* Skip empty lines and lines starting with '#' (comments)  *)
          if String.length line = 0 || line.[0] = '#' then
            loop l
          else begin
            match split_line line with
                [i; j] ->
                  let v = vertex_number i and w = vertex_number j in
		    if v = w
		    then failwith "bad self-loop";
                    loop ((v, w) :: l)
              | _ -> failwith "Bad edge syntax"
          end
      with End_of_file -> l
    in
      loop [] in
  let g =
    List.fold_left (fun g (v, w) -> Graph.connect g v w) Graph.empty edges
  in
    g, vertex_names
;;

let usage_msg = "Find edge clique covers";;

let complement_graph = ref false;;

let specs = [
  ("-c", Arg.Set(complement_graph),
         "Work on complement graph");
  ("-v", Arg.Set(Util.verbose),
         "Print progress to stderr");
];;

let print_cliques cliques vertex_names =
  let cliques = List.map
    (fun c -> (IntSet.fold
		 (fun cliques i -> (Hashtbl.find vertex_names i) :: cliques)
		 c []))
    cliques in
  let cliques = List.map (List.sort compare) cliques in
  let cliques = List.sort compare cliques in
    List.iter
      (fun c ->
	 ignore (List.fold_left
		   (fun first i ->
		      if (not first) then print_char ' ';
		      print_string i;
		      false)
		   true
		   c);
	 print_newline ())
      cliques
;;

let is_clique_cover g cliques =
  let g = List.fold_left Graph.clear_subgraph g cliques in
    Graph.num_edges g = 0
;;

let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;  
  let g, vertex_names = read_graph () in
  let g = if !complement_graph then Graph.complement g else g in
    Graph.dump g;
  let rec loop k =
    if !Util.verbose then Printf.eprintf "*** k = %d ***\n%!" k;
    match Branch.ecc_solve g k with
	None -> loop (k + 1)
      | Some cliques ->
	  assert (List.length cliques = k);
	  if !Util.verbose then Printf.eprintf "Found solution with k = %d cliques\n%!" k;
	  print_cliques cliques vertex_names;
	  if not (is_clique_cover g cliques) then begin
	    Printf.fprintf stderr "VERIFICATION FAILED!!1!\n%!";
	    exit 1;
	  end
  in
    loop 0
;;
