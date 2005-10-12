(** A module for sets of integers represented as big endian patricia tree.  *)
    
type t
(** The type of the set.  *)


val empty : t
(** The empty set.  *)

val singleton : int -> t
(** Returns a one-element set.  *)

val of_list : int list -> t
(** Returns a set containing all elements of the list.  *)


val is_empty : t -> bool
(** Test whether the set is empty (O(1)).  *)

val size : t -> int
(** Returns the number of elements of a set (O(1)).  *)

val equal : t -> t -> bool
(** [equal s1 s2] is true when [s1] and [s2] contain the same elements.  *)

val contains : t -> int -> bool
(** [contains s i] returns true if [i] is element of [s] (O(log n)).  *)

val is_subset : t -> t -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of the set [s2].  *)

val choose : t -> int
(** Return one element of the given set, or raise [Not_found] if the set
    is empty.  *)

val add : t -> int -> t
(** [add x s] returns a set containing all elements of [s], plus
    [x]. If [x] was already in [s], [s] is returned unchanged.  *)

val remove : t -> int -> t
(** [remove x s] returns a set containing all elements of [s] except
    [x]. If [x] is not in [s], [s] is returned unchanged.  *)

val pop : t -> int * t

val union : t -> t -> t
(** Set union.  *)

val minus : t -> t -> t
(** Set difference.  *)
    
val intersection : t -> t -> t
(** Set intersection.  *)

val do_intersect : t -> t -> bool
(** True if the two sets have a nonempty intersection.  *)

val intersection_size : t -> t -> int
(** Number of elements in the intersection.  *)

val split : t -> int -> t * bool * t
(** [split x s] returns a triple [(l, present, r)], where
    [l] is the set of elements of [s] that are strictly less than [x];
    [r] is the set of elements of [s] that are strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x], or
    [true] if [s] contains an element equal to [x].  *)

val partition : (int -> bool) -> t -> t * t
(** [partition p s] returns a pair of sets [(s1, s2)], where [s1] is
    the set of all the elements of [s] that satisfy the predicate [p],
    and [s2] is the set of all the elements of [s] that do not satisfy
    [p].  *)
    

val find_opt : (int -> 'a option) -> t -> 'a option
(** [find_opt p s] returns [Some i] when [p] returns [Some i] for some
    element of [s], or [None] if [p] returns [None] for all elements of
    [s].  *)

val find : (int -> bool) -> t -> int
(** [find p s] returns [i] when [p] returns [true] for some element of
    [s], or raises [Not_found] if [p] returns [false] for all elements
    of [s].  *)


val for_all : (int -> bool) -> t -> bool

val fold : ('a -> int -> 'a) -> t -> 'a -> 'a
val fold_intersection : ('a -> int -> 'a) -> t -> t -> 'a -> 'a
val iter : (int -> unit) -> t -> unit

val output : out_channel -> t -> unit
val print : t -> unit
val dump : t -> unit
