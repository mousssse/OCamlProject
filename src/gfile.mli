(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> string graph

(* These are specifically for mbp and bg graphs, not any graphs, values are read as int *)
val from_mbp_file: path -> string * string * int graph
val from_bg_file: path -> (int * int) graph

(* We write only a string graph. *)
val write_file: path -> string graph -> unit

(* Exporting the graph to a dot format graph file.
   To visualise the graph, use http://magjac.com/graphviz-visual-editor/ *)
val export: path -> string graph -> unit
