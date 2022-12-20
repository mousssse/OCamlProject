open Graph

val clone_nodes: 'a graph -> 'b graph
val clone_nodes_without: id list -> 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph