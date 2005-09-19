module M = Map.Make(struct type t = int let compare = compare end);;

type 'a t = 'a M.t;;

let empty = M.empty;;

let size m = M.fold (fun _ _ size -> size + 1) m 0;;
let has_key m k = M.mem k m;;
let get m v = M.find v m;;
let get_default m v d = try get m v with Not_found -> d;;

let add m k v = M.add k v m;;

let map = M.mapi;;
let iter = M.iter;;

let fold f m accu = M.fold (fun k v accu -> f accu k v) m accu;;

let fold_inorder f m accu =
  let data =
    fold
      (fun data k v -> (k, v) :: data)
      m [] in
  let data = List.sort compare data in
    List.fold_left (fun accu (k, v) -> f accu k v) accu data
;;

exception Got_it;;
let find p m =
  let it = ref None in
  try
    iter (fun k v ->
      let r = p k v in
        if r <> None then begin
          it := r;
	  raise Got_it;
        end) m;
    None;
  with Got_it -> !it
;;
