open Graph

(* the label on an arc is the flow and the cost of the arc *)
type label = int * int
type path = id list

(* find_path gr id1 id2 
 *   returns (None, max_int) if no path can be found.
 *   returns (Some p, c) if a path p of cost c from id1 to id2 has been found. 
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