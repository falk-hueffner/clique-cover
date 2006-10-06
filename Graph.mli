(** Module for handling undirected functional graphs.
    Vertices are represented by integers. No self loops or multiple
    edges are possible.  *)

type t
(** The type of a graph.  *)

val empty : t
(** Returns the empty graph. O(1) time.  *)

val of_graph6 : string -> t
val make_clique : int -> t

val num_vertices : t -> int
(** Returns the number of vertices in the graph. O(1) time.  *)

val num_edges : t -> int
(** Returns the number of edges in the graph. O(n) time.  *)
  
val has_vertex : t -> int -> bool
(** [has_vertex g i] is true if vertex [i] is present in graph
    [g]. O(log n) time.  *) 
  
val vertices : t -> IntSet.t
(** Returns the set of vertices. O(n) time.  *)
  
val is_connected : t -> int -> int -> bool
(** [is_connected g i j] returns true if there is an edge between
    [i] and [j] in [g]. O(log n) time. *)

val neighbors : t -> int -> IntSet.t
(** [neighbors g i] returns the set of neighbors of [i] in
    [g]. O(log n) time.  *)

val is_deg0 : t -> int -> bool
(** [is_deg0 g i] returns true if [i] has no neighbors in [g]. O(log n)
    time.  *)

val deg : t -> int -> int
(** [deg g i] returns the number of neighbors of [i] in [g]. O(log
    n) time.  *)
  
val is_clique : t -> bool
val choose_edge : t -> int * int

val add_vertex : t -> int -> t
(** [add_vertex g i] returns [g] with a new degree-0 vertex [i].  *)

val new_vertex : t -> int
(** [new_vertex g] returns the smallest positive [i] such that [i] is not a vertex
    in [g].  *)

val connect : t -> int -> int -> t
(** [connect g v w] returns [g] with vertices [v] and [w]
    connected. O(log n) time.  *) 

val disconnect : t -> int -> int -> t
(** [disconnect g v w] returns [g] with vertices [v] and [w]
    disconnected.  O(log n) time.  *) 

val delete_vertex : t -> int -> t
(** [delete_vertex g i] returns [g] with vertex [i] deleted. O(m)
    time.  *)

val subgraph : t -> IntSet.t -> t
val clear_subgraph : t -> IntSet.t -> t
val complete_subgraph : t -> IntSet.t -> t
val num_edges_in_subgraph : t -> IntSet.t -> int

val fold_vertices : ('a -> int -> IntSet.t -> 'a) -> t -> 'a -> 'a
val fold_neighbors : ('a -> int -> 'a) -> t -> int -> 'a -> 'a
val fold_edges : ('a -> int -> int -> 'a) -> t -> 'a -> 'a
val fold_subgraph_edges : ('a -> int -> int -> 'a) -> t -> IntSet.t -> 'a -> 'a

val iter_edges : (int -> int -> unit) -> t -> unit
(** [iter_edges f g] calls [f u v] for each edge [(u, v)] in [g].  *)

val complement : t -> t

val output : out_channel -> t -> unit
(** [output c g] prints a debug representation of [g] to channel [c].  *)

val print : t -> unit
(** [print g] is the same as [output stdout g].  *)

val dump : t -> unit
(** [dump g] is the same as [output stderr g].  *)
