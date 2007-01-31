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

let usage_msg = "Find optimal edge clique covers";;

let complement_graph = ref false;;
let stats_only       = ref false;;
let ksw	             = ref false;;
let insert_absorb    = ref false;;
let sweep            = ref false;;
let cover_singletons = ref false;;

let specs = [
  ("-s", Arg.Set(stats_only),
         "Print statistics only");
  ("-c", Arg.Set(complement_graph),
         "Work on complement graph");
  ("-k", Arg.Set(ksw),
         "Use heuristic by Kellerman");
  ("-i", Arg.Set(insert_absorb),
         "Use Insert-Absorb heuristic by Piepho");
  ("-w", Arg.Set(sweep),
         "Do sweeping");
  ("-g", Arg.Set(cover_singletons),
         "Cover singletons");
  ("-v", Arg.Set(Util.verbose),
         "Print progress to stderr");
  ("-1", Arg.Clear(ECC.use_rule1),
         "Disable Rule 1");
  ("-2", Arg.Clear(ECC.use_rule2),
         "Disable Rule 2");
  ("-3", Arg.Clear(ECC.use_rule3),
         "Disable Rule 3");
  ("-4", Arg.Clear(ECC.use_rule4),
         "Disable Rule 4");
]
;;

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

let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  if !ksw && !insert_absorb then begin
    prerr_string "Cannot use both Kellerman and Insert-Absorb\n";
    exit 1;
  end;
  let g, vertex_names = read_graph () in
  let g = if !complement_graph then Graph.complement g else g in
  let g', singletons =
    Graph.fold_vertices
      (fun (g', singletons) i neighbors ->
	 if IntSet.is_empty neighbors
	 then (Graph.delete_vertex g' i), (IntSet.add singletons i)
	 else g', singletons)
      g (g, IntSet.empty) in
  if !stats_only
  then Printf.printf "%3d %4d%!"
	(Graph.num_vertices g) (Graph.num_edges g);
  let start = Util.timer () in
  let cliques =
    if !ksw
    then KSW.ecc_heuristic g'
    else if !insert_absorb
    then InsertAbsorb.ecc_heuristic g'
    else Branch.ecc_solve g' in
  let cliques = if !sweep then Sweep.sweep g cliques else cliques in
  let stop = Util.timer () in
  let cliques =
    if !cover_singletons
    then IntSet.fold (fun cliques i -> (IntSet.singleton i) :: cliques) singletons cliques
    else cliques in    
  let ones = List.fold_left (fun ones c -> ones + (IntSet.size c)) 0 cliques in
    begin
    if not !stats_only
    then print_cliques cliques vertex_names
    else Printf.printf " %4d %5d %8.2f %8Ld %8Ld %8Ld %8Ld %1Ld\n"
	(List.length cliques) ones (stop -. start) !Branch.branch_calls
	!ECC.rule1_counter !ECC.rule2_counter !ECC.rule3_counter !ECC.rule4_counter;
    if not (ECC.is_clique_cover g cliques) then begin
      Printf.fprintf stderr "internal error: verification failed\n%!";
      exit 1;
    end
  end
;;
