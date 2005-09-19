val verbose : bool ref
val output_list : (out_channel -> 'a -> 'b) -> out_channel -> 'a list -> unit
val print_list : (out_channel -> 'a -> 'b) -> 'a list -> unit
val dump_list : (out_channel -> 'a -> 'b) -> 'a list -> unit
