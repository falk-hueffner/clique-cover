(** Module for handling undirected functional graphs.  *)

type t
(** The type of a graph.  *)

val empty : t
(** Returns the empty graph.  *)

val num_vertices : t -> int
val num_edges : t -> int
val vertices : t -> IntSet.t
val is_connected : t -> int -> int -> bool
val neighbors : t -> int -> IntSet.t
val is_deg0 : t -> int -> bool
val is_clique : t -> bool
val choose_edge : t -> int * int

val connect : t -> int -> int -> t
(** [connect g v w] connects vertices [v] and [w] in [g]. Takes O(log n) time.  *)

val delete_vertex : t -> int -> t
  
val subgraph : t -> IntSet.t -> t
val clear_subgraph : t -> IntSet.t -> t

val find_edge_opt : (int -> int -> 'a option) -> t -> 'a option

val fold_vertices : ('a -> int -> IntSet.t -> 'a) -> t -> 'a -> 'a
val fold_vertices_inorder : ('a -> int -> IntSet.t -> 'a) -> t -> 'a -> 'a

val fold_neighbors : ('a -> int -> 'a) -> t -> int -> 'a -> 'a

val fold_edges : ('a -> int -> int -> 'a) -> t -> 'a -> 'a

val iter_edges : (int -> int -> unit) -> t -> unit
(** [iter_edges f g] calls [f u v] for each edge [(u, v)] in [g].  *)

val map : (int -> IntSet.t -> IntSet.t) -> t -> t

val complement : t -> t

val output : out_channel -> t -> unit
(** [output c g] prints a debug representation of [g] to channel [c].  *)

val print : t -> unit
(** [print g] is the same as [output stdout g].  *)

val dump : t -> unit
(** [dump g] is the same as [output stderr g].  *)
