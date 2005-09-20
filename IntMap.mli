type 'a t

val empty : 'a t

val size : 'a t -> int
val has_key : 'a t -> int -> bool
val get : 'a t -> int -> 'a
val get_default : 'a t -> int -> 'a -> 'a

val add : 'a t -> int -> 'a -> 'a t

val fold : ('a -> int -> 'b -> 'a) -> 'b t -> 'a -> 'a
val fold_inorder : ('a -> int -> 'b -> 'a) -> 'b t -> 'a -> 'a
val map : (int -> 'a -> 'b) -> 'a t -> 'b t
val find : (int -> 'a -> bool) -> 'a t -> (int * 'a)
val find_opt : (int -> 'a -> 'b option) -> 'a t -> 'b option
