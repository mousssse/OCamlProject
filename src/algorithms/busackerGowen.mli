open Graph

(* the label on an arc is the flow and the cost of the arc *)
type label = int * int
type path = id list

(* find_path gr id1 id2 
 *   returns (None, max_int) if no path can be found.
 *   returns (Some p, c) if a path p of minimal cost c from id1 to id2 has been found. 
*)
val find_path: label graph -> id -> id -> path option * int

(* path_to_string source path 
 *   returns the string representation of the given path
*)
val path_to_string: id -> path option -> string

(* get_min_flow_path gr source path 
 *   returns the minimum flow value in the given path
*)
val get_min_flow_path: label graph -> id -> path -> int

(* build_residual_graph gr flow source path 
 *   returns the residual graph, where costs and flows of the arcs of the path have been updated
*)
val build_residual_graph: label graph -> int -> id -> path -> label graph

(* busackerGowen gr source sink 
 *   returns the final residual graph of the busacker gowen algorithm
*)
val busackerGowen: label graph -> id -> id -> label graph * int * int

(* get_final_string_graph init_graph bg_graph 
 *   returns a clean string graph version of the result of the busacker gowen algorithm
*)
val get_final_string_graph: label graph -> label graph -> string graph