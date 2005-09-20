(** Module for handling undirected functional graphs.  *)

type t
(** The type of a graph.  *)

type vertex = int;;

val empty : t
(** Returns the empty graph.  *)

val num_edges : t -> int
val vertices : t -> IntSet.t
val is_connected : t -> int -> int -> bool
val neighbors : t -> int -> IntSet.t
val is_clique : t -> bool
val choose_edge : t -> int * int

val connect : t -> vertex -> vertex -> t
(** [connect g v w] connects vertices [v] and [w] in [g]. Takes O(log n) time.  *)

val subgraph : t -> IntSet.t -> t
val clear_subgraph : t -> IntSet.t -> t

val find_edge_opt : (int -> int -> 'a option) -> t -> 'a option

val fold_vertices : ('a -> vertex -> IntSet.t -> 'a) -> t -> 'a -> 'a
val fold_vertices_inorder : ('a -> vertex -> IntSet.t -> 'a) -> t -> 'a -> 'a

val fold_neighbors : ('a -> vertex -> 'a) -> t -> vertex -> 'a -> 'a

val fold_edges : ('a -> vertex -> vertex -> 'a) -> t -> 'a -> 'a

val iter_edges : (vertex -> vertex -> unit) -> t -> unit
(** [iter_edges f g] calls [f u v] for each edge [(u, v)] in [g].  *)

val map : (int -> IntSet.t -> IntSet.t) -> t -> t

val complement : t -> t

val output : out_channel -> t -> unit
(** [output c g] prints a debug representation of [g] to channel [c].  *)

val print : t -> unit
(** [print g] is the same as [output stdout g].  *)

val dump : t -> unit
(** [dump g] is the same as [output stderr g].  *)
