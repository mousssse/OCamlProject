open Graph

(* path is a list of successive nodes *)
type path = id list

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
*)
(*val path_to_string : id -> path option -> string*)
val find_path: int graph -> id list -> id -> id -> path option
val get_min_flow_path: int graph -> int -> id -> path -> int
val build_residual_graph: int graph -> int -> id -> path -> int graph
val fordfulkerson: int graph -> int -> int -> int graph
val get_final_string_graph: int graph -> int graph -> string graph