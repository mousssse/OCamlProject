open Graph
open Tools
open Printf

type path = id list


(* Converts the path into a string so that we can print it *)
let path_to_string (source: id) (path_option: path option) = 
  match path_option with
  | None -> "No path found"
  | Some(path) ->
    let rec loop = function
      | [] -> ""
      | id :: [] -> string_of_int id
      | id :: rest_path -> (string_of_int id) ^ " -> " ^ (loop rest_path)
    in
    (string_of_int source) ^ " -> " ^ loop path


(* returns a path between two nodes, or None if such a path doesn't exist *)
let rec find_path (gr: int graph) (forbidden_nodes: id list) (from_id: id) (to_id: id) =
  let rec loop out_arcs = 
    match out_arcs with
    | [] -> None
    | (id, label) :: rest_arcs ->
      (* flow is null *)
      if label == 0 then loop rest_arcs
      (* node has already been visited *)
      else if List.mem id forbidden_nodes then loop rest_arcs
      (* we found the sink node we want to go to! *)
      else if id == to_id then Some(List.append forbidden_nodes [id])
      else 
        (* calling find_path from intermediary node *)
        let following_path = find_path gr (List.append forbidden_nodes [id] ) id to_id in
        if following_path = None then loop rest_arcs else following_path
  in
  
  let out_arcs = out_arcs gr from_id in
  loop out_arcs


(* Finds the minimum flow value in the path given *)
let rec get_min_flow_path (graph: int graph) (acu: int) (from_id: id) = function
  | [] -> acu
  | to_id :: rest_path ->
    let flow = find_arc graph from_id to_id in
    match flow with
    | None -> 0 (* TODO: think abt this more *)
    | Some(value) -> get_min_flow_path graph (min acu value) to_id rest_path


(* the init graph will create itself *)
(* Updates in and out arcs on the path with the update of flow value to build the new residual graph *)
let rec build_residual_graph (graph: int graph) (flow_val: int) (from_id: id) = function
    | [] -> graph
    | to_id :: rest_path ->
      let update_in_arc = add_arc graph to_id from_id flow_val in
      let update_out_arc = add_arc update_in_arc from_id to_id (-flow_val) in
      build_residual_graph update_out_arc flow_val to_id rest_path


let init_acu (graph: int graph) (source: id) (first_id_in_path: id) =
  match find_arc graph source first_id_in_path with
  | None -> 1000000
  | Some(flow) -> flow

let fordfulkerson (graph: int graph) (source: id) (destination: id) = 
  let rec loop gr src dest =
    let path_option = find_path gr [] src dest in
    match path_option with
    | None -> gr
    | Some(path) ->
      let flow_val = get_min_flow_path gr (init_acu gr src (List.hd path)) src path in
      let residual_graph = build_residual_graph gr flow_val src path in
      (* to check *)
      Printf.printf "path: %s\n%!" (path_to_string src path_option);
      loop residual_graph src dest
  in

  loop graph source destination