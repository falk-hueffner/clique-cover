let verbose = ref false;;

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

let rec list_find_opt p = function
    [] -> None
  | x :: xs ->
      let r = p x in
	if r <> None then r else list_find_opt p xs
;;

let print_list printer l = output_list printer stdout l;;
let dump_list printer l = output_list printer stderr l;;
