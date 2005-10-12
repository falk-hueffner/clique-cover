(** A module for maps of integers represented as big endian patricia tree.  *)

type 'a t
(** The type of the map.  *)

val empty : 'a t
(** The empty map.  *)

val is_empty : 'a t -> bool

val size : 'a t -> int
(** Returns the number of elements of a set (O(1)).  *)

val has_key : 'a t -> int -> bool
(** [has_key m i] returns true if there is a mapping [(i, x)] in [m]
    (O(log n)).  *)

val max_key : 'a t -> int

val get : 'a t -> int -> 'a
(** [get m i] returns the current binding of [i] in [m], or raises
    [Not_found] if no such binding exists.  *)

val get_default : 'a t -> int -> 'a -> 'a
(** [get_default m i x] returns the current binding of [i] in [m], or
    [x] if no such binding exists.  *)

val add : 'a t -> int -> 'a -> 'a t
(** [add m i x] returns a map containing the same bindings as [m],
    plus a binding of [i] to [x]. If [x] was already bound in [m],
    its previous binding disappears.  *)
  
val remove : 'a t -> int -> 'a t
(** [remove m i]returns a map containing the same bindings as [m],
    except for [i] which is unbound in the returned map.  *)

val update : ('a -> 'a) -> 'a t -> int -> 'a t

val fold : ('a -> int -> 'b -> 'a) -> 'b t -> 'a -> 'a
(** [fold f m a] computes [(f iN xN ... (f i1 x1 a)...)], where [i1
    ... In] are the keys of all bindings in [m] (in increasing
    order), and [x1 ... xN] are the associated data.  *)

val iter : (int -> 'a -> unit) -> 'a t -> unit

val map : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [map f m] returns a map with same domain as [m], where the
    associated value [x] of all bindings of [m] has been replaced by the
    result of the application of [f] to [a]. The bindings are passed to
    [f] in increasing order of the keys.  *)

val find : (int -> 'a -> bool) -> 'a t -> (int * 'a)
(** [find p m] returns [(i, x)] when [p] returns [true] for some
    binding [(i, x)] in [m], or raises [Not_found] if [p] returns
    [false] for all elements of [m].  *)

val for_all : (int -> 'a -> bool) -> 'a t -> bool
(** [for_all p m] checks whether all bindings of [m] satisfy the
    predicate [p].  *)

val output : (out_channel -> 'a -> unit) -> out_channel -> 'a t -> unit
val print : (out_channel -> 'a -> unit) -> 'a t -> unit
val dump : (out_channel -> 'a -> unit) -> 'a t -> unit
