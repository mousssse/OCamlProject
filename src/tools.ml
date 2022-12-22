open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

(* clone nodes without cloning the nodes that have their id in the list *)
let clone_nodes_without list graph =
  n_fold graph (fun graph id -> if List.mem id list then graph else new_node graph id) empty_graph

let gmap gr f = 
  let nodes = clone_nodes gr in
  let g graph id1 id2 cost = new_arc graph id1 id2 (f cost) in
  e_fold gr g nodes

let add_arc graph id1 id2 n =
  match find_arc graph id1 id2 with 
  | None -> new_arc graph id1 id2 n
  | Some(label) -> new_arc graph id1 id2 (n + label)

let del_arc graph from_id to_id =

  let create_arc_if_other graph id1 id2 label from_id to_id =
    if id1 = from_id && id2 = to_id then graph 
    else new_arc graph id1 id2 label
  in

  match find_arc graph from_id to_id with 
  | None -> raise (Graph_error ("No arc from node #" ^ string_of_int from_id ^ " to node #" ^string_of_int to_id))
  | Some(_) ->
    let new_graph = clone_nodes graph in 
    e_fold graph (fun gr id1 id2 lbl -> create_arc_if_other gr id1 id2 lbl from_id to_id) new_graph