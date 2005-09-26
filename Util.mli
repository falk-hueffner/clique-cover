val verbose : bool ref
val timer : unit -> float
val fold_n : ('a -> int -> 'a) -> int -> 'a -> 'a
val list_find_opt : ('a -> 'b option) -> 'a list -> 'b option
val output_list : (out_channel -> 'a -> 'b) -> out_channel -> 'a list -> unit
val print_list : (out_channel -> 'a -> 'b) -> 'a list -> unit
val dump_list : (out_channel -> 'a -> 'b) -> 'a list -> unit
