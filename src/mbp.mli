open Graph

val clean_graph: 'a graph -> 'a graph
val add_source_and_sink: int graph -> id * id * int graph
val mbp: int graph -> string graph * int