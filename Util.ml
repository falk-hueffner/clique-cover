let verbose = ref false;;

let timer () =
  let stamp = Unix.times () in
    stamp.Unix.tms_utime;;

let output_int channel i = Printf.fprintf channel "%d" i;;

let output_list printer channel l =
  begin
    output_char channel '[';
    let rec loop l =
      match l with
          [] -> output_char channel ']'
        | [ x ] -> printer channel x; output_char channel ']';
        | x :: xs -> printer channel x; output_char channel ' '; loop xs
    in
      loop l
  end;;

let fold_n f n accu =
  let rec loop accu i =
    if i >= n
    then accu
    else loop (f accu i) (succ i)
  in
    loop accu 0
;;

let rec list_find_opt p = function
    [] -> None
  | x :: xs ->
      let r = p x in
	if r <> None then r else list_find_opt p xs
;;

let int64_incr x = x := Int64.succ !x;;

let print_list printer l = output_list printer stdout l;;
let dump_list printer l = output_list printer stderr l;;
