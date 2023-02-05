open Graph
open Tools
open Fordfulkerson

let available_id graph =
  let rec loop graph acu =
    if node_exists graph acu then loop graph (acu+1)
    else acu
  in
  loop graph 0


let clean_graph graph =

  (* function that returns true if the node has no out arcs nor in arcs*)
  let node_is_disconnected graph id = 
    if (out_arcs graph id) = [] then 
        n_fold graph (fun res from_id -> if (find_arc graph from_id id) = None then res else false) true
    else false
  in

  (* the list of nodes that have no out arcs nor in arcs*)
  let disconnected_nodes = 
    n_fold graph (fun liste id -> if node_is_disconnected graph id then id :: liste else liste) []
  in
  
  (* new_graph is the given graph without the disconnected nodes and with the same arcs*)
  let new_graph = clone_nodes_without disconnected_nodes graph in
  let new_graph = e_fold graph new_arc new_graph in
  new_graph 


(* adding and connecting the source and sink nodes in the graph *)
let add_source_and_sink graph =
  (* getting ids + creating source and sink nodes *)
  let source_id = available_id graph in
  let new_graph = new_node graph source_id in
  let sink_id = available_id new_graph in
  let new_graph = new_node new_graph sink_id in
  (* creating all the arcs *)
  (
    source_id, 
    sink_id, 
    n_fold graph (fun gr id -> if out_arcs graph id = [] then new_arc gr id sink_id 1 else new_arc gr source_id id 1) new_graph
  )


let mbp graph = 
  let (source, sink, graph) = add_source_and_sink (clean_graph graph) in
  let mbp_graph = get_final_string_graph graph (fordfulkerson graph source sink) in

  (* getting a pretty final graph without source/sink nodes, without 0/1 arcs and with label-less arcs *)
  let final_graph = clone_nodes_without [source ; sink] mbp_graph in
  let final_graph = e_fold mbp_graph (fun gr id1 id2 lbl -> 
                                        if id1 = source then gr 
                                        else if id2 = sink then gr 
                                        else if String.contains_from lbl 0 '0' then gr
                                        else new_arc gr id1 id2 "\"\"") final_graph in

  let mbp_val = n_fold final_graph (fun mbp id -> mbp + 1) 0 in
  (final_graph, mbp_val)