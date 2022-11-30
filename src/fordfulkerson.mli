open Graph

(* path is a list of successive nodes *)
type path = id list

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
 *)
 val path_to_file : string -> path -> unit
 val find_path: int graph -> id list -> id -> id -> path option
