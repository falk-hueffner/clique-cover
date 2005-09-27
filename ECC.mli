type t

val make : Graph.t -> t

val g : t -> Graph.t
val k : t -> int
val uncovered : t -> Graph.t

val all_covered : t -> bool
val branching_edge : t -> int * int

val cover : t -> IntSet.t -> t
