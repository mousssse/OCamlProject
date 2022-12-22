open Graph
open Tools
open Printf

type path = id list
exception Path_error of string


(* returns a path between two nodes, or None if such a path doesn't exist *)
let find_path (gr: int graph) (from_id: id) (to_id: id) =

  let rec find gr visited from_id to_id =
    let rec loop = function
      | [] -> None
      | (id, flow) :: rest_arcs ->

        (* flow is null *)
        if flow == 0 then loop rest_arcs

        (* node has already been visited *)
        else if List.mem id visited then loop rest_arcs

        (* we found the destination node! *)
        else if id == to_id then Some(List.append visited [id])

        else 
          (* intermediary node *)
          let following_path = find gr (List.append visited [id]) id to_id in
          if following_path = None then loop rest_arcs else following_path
    in

    let out_arcs = out_arcs gr from_id in
    loop out_arcs
  in
  find gr [] from_id to_id


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


(* Finds the minimum flow value in the path given *)
let get_min_flow_path (graph: int graph) (from_id: id) (path: path) = 
  
  (* returns the flow value of the first arc in the path *)
  let init_acu graph source first_id_in_path =
    match find_arc graph source first_id_in_path with
    | None -> raise (Path_error ("The path given doesn't exist in the graph. No arc from source #" ^ string_of_int from_id ^ " to #" ^ string_of_int first_id_in_path))
    | Some(flow) -> flow 
  in
  
  let rec loop graph acu from_id = function
  | [] -> acu
  | to_id :: rest_path ->
    match find_arc graph from_id to_id with
    | None -> raise (Path_error ("The path given doesn't exist in the graph. No arc from #" ^ string_of_int from_id ^ " to #" ^ string_of_int to_id))
    | Some(flow) -> loop graph (min acu flow) to_id rest_path
  in
  loop graph (init_acu graph from_id (List.hd path)) from_id path


(* Updates in and out arcs on the path with the update of flow value to build the new residual graph *)
let rec build_residual_graph (graph: int graph) (flow_val: int) (from_id: id) = function
  | [] -> graph
  | to_id :: rest_path ->
    let update_in_arc = add_arc graph to_id from_id flow_val in
    let update_out_arc = add_arc update_in_arc from_id to_id (-flow_val) in
    build_residual_graph update_out_arc flow_val to_id rest_path


let rec fordfulkerson (graph: int graph) (source: id) (destination: id) = 
  let path_option = find_path graph source destination in
  match path_option with
  | None -> graph (* Done *)
  | Some(path) ->
    let flow_val = get_min_flow_path graph source path in
    let residual_graph = build_residual_graph graph flow_val source path in
    (*Printf.printf "path: %s\n%!" (path_to_string src path_option);*)
    fordfulkerson residual_graph source destination


(* creates a string graph from the final flow graph with max_flow/capacity labelled arcs *)
let get_final_string_graph (init_graph: int graph) (ff_graph: int graph) = 

  (* For a given flow arc, will create the flow/capacity arc on the initial graph *)
  let create_arcs init_graph from_id to_id flow =
    (* The arcs containing the flow are the ones that are in the opposite directions
       from the arcs inside of the initial graph *)
    match find_arc init_graph to_id from_id with 
    | Some(capacity) -> new_arc (init_graph) (to_id) (from_id) ("\"" ^ flow ^ "/" ^ capacity ^ "\"")
    | None -> init_graph
  in

  (* For the arcs with no flow going through them, will create a 0/capacity label *)
  let create_null_flow_arcs final_graph from_id to_id capacity =
    (* The arcs with no flow are the ones that haven't changed in the inital graph, they dont contain a '/' yet *)
    if String.contains_from capacity 0 '/' then final_graph
    else new_arc (final_graph) (from_id) (to_id) ("\"0/" ^ capacity ^ "\"")
  in

  let string_ff_graph = (gmap ff_graph string_of_int) in 
  let final_graph = e_fold string_ff_graph create_arcs (gmap init_graph string_of_int) in
  e_fold final_graph create_null_flow_arcs final_graph