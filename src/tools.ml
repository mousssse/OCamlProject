open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = 
  let nodes = clone_nodes gr in
  let g graph id1 id2 cost = new_arc graph id1 id2 (f cost) in
  e_fold gr g nodes

let add_arc graph id1 id2 n =
  match find_arc graph id1 id2 with 
  | None -> new_arc graph id1 id2 n
  | Some(label) -> new_arc graph id1 id2 (n + label)