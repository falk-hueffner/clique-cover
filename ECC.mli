type t

val is_clique_cover : Graph.t -> IntSet.t list -> bool
  
val make : Graph.t -> t

val g : t -> Graph.t
val k : t -> int
val k_used_up : t -> bool
val all_covered : t -> bool
val branching_edge : t -> int * int
val restore : t -> IntSet.t list -> IntSet.t list

val set_max_k : t -> int -> t
  
val cover : t -> IntSet.t -> t
val reduce : t -> t

val use_rule1 : bool ref
val use_rule2 : bool ref
val use_rule3 : bool ref
val use_rule4 : bool ref

val rule1_counter : Int64.t ref
val rule2_counter : Int64.t ref
val rule3_counter : Int64.t ref
val rule4_counter : Int64.t ref
