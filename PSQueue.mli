type 'a t

val empty : 'a t
    
val add : 'a t -> int -> 'a -> int -> 'a t
val is_empty : 'a t -> bool
val top : 'a t -> int * 'a * int
val get : 'a t -> int -> 'a * int
val pop : 'a t -> 'a t
val remove : 'a t -> int -> 'a t

val fold : ('b -> int -> 'a -> int -> 'b) -> 'a t -> 'b -> 'b
