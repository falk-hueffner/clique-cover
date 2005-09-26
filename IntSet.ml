module S = Set.Make(struct type t = int let compare = compare end);;

type t = S.t;;

let empty = S.empty;;
let singleton = S.singleton;;

let is_empty = S.is_empty;;
let size s = S.fold (fun _ size -> size + 1) s 0;;
let contains s x = S.mem x s;;
let is_subset s1 s2 = S.subset s1 s2;;
let choose = S.choose;;

let add s x = S.add x s;;
let remove s x = S.remove x s;;
let union = S.union;;
let minus = S.diff;;
let intersection s1 s2 = S.inter s1 s2;;
let intersection_size s1 s2 = S.cardinal (S.inter s1 s2);;
let split s x = S.split x s;;
let partition p s = S.partition p s;;

let fold f s accu = S.fold (fun i accu -> f accu i) s accu;;
let iter = S.iter;;

exception Got_it;;
let find_opt p s =
  let it = ref None in
  try
    iter (fun i ->
      let r = p i in
        if r <> None then begin
          it := r;
	  raise Got_it;
        end) s;
    None;
  with Got_it -> !it
;;

let find p s =
  match find_opt (fun x -> if p x then Some x else None) s with
      None -> raise Not_found
    | Some x -> x
;;

let output channel s =
  Printf.fprintf channel "{";
  iter (fun i -> Printf.fprintf channel " %d" i) s;
  Printf.fprintf channel " }";
;;

let print s = output stdout s;;
let dump  s = output stderr s;;
