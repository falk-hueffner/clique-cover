type t

val empty : t
val singleton : int -> t

val is_empty : t -> bool
val size : t -> int
val contains : t -> int -> bool
val is_subset : t -> t -> bool
val choose: t -> int

val add : t -> int -> t
val remove : t -> int -> t
val union : t -> t -> t
val minus : t -> t -> t
val intersection : t -> t -> t
val intersection_size : t -> t -> int
val split : t -> int -> t * bool * t
val partition : (int -> bool) -> t -> t * t

val fold : ('a -> int -> 'a) -> t -> 'a -> 'a
val iter : (int -> unit) -> t -> unit
val find : (int -> bool) -> t -> int
val find_opt : (int -> 'a option) -> t -> 'a option

val output : out_channel -> t -> unit
val print : t -> unit
val dump : t -> unit
