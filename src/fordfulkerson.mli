open Graph

(* path is a list of successive nodes *)
type path = id list

(* find_path gr id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
*)
val find_path: int graph -> id -> id -> path option

(* path_to_string source path 
 *   returns the string representation of the given path
*)
val path_to_string : id -> path option -> string

(* get_min_flow_path gr source path 
 *   returns the minimum flow value in the given path
*)
val get_min_flow_path: int graph -> id -> path -> int

(* build_residual_graph gr flow source path 
 *   returns the residual graph, updated with +/- flow on the arcs of the path
*)
val build_residual_graph: int graph -> int -> id -> path -> int graph

(* fordfulkerson gr source sink 
 *   returns the final residual graph of the ford fulkerson algorithm
*)
val fordfulkerson: int graph -> int -> int -> int graph

(* get_final_string_graph init_graph ff_graph 
 *   returns a clean string graph version of the result of the ford fulkerson algorithm
*)
val get_final_string_graph: int graph -> int graph -> string graph