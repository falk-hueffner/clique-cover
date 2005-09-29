type t

val make : Graph.t -> t * (IntSet.t list -> IntSet.t list)

val g : t -> Graph.t
val k : t -> int
val k_used_up : t -> bool
val uncovered : t -> Graph.t

val all_covered : t -> bool
val branching_edge : t -> int * int

val set_max_k : t -> int -> t
  
val cover : t -> IntSet.t -> t * (IntSet.t list -> IntSet.t list)
