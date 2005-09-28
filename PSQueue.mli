type 'a t

exception Empty

type key = int * int

val empty : 'a t
    
val add : 'a t -> key -> 'a -> int -> 'a t
val is_empty : 'a t -> bool
val top : 'a t -> key * 'a * int
val get : 'a t -> key -> 'a * int
val pop : 'a t -> 'a t
val remove : 'a t -> key -> 'a t

val fold : ('b -> key -> 'a -> int -> 'b) -> 'a t -> 'b -> 'b
