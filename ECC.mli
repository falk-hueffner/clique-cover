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

val use_rule1 : bool ref
val use_rule2 : bool ref
val use_rule3 : bool ref
val use_rule4 : bool ref

val rule1_counter : Int64.t ref
val rule2_counter : Int64.t ref
val rule3_counter : Int64.t ref
val rule4_counter : Int64.t ref
