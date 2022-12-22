open Graph
open Tools

type label = int * int
type path = id list
exception Path_error of string


(* returns a path between two nodes, or None if such a path doesn't exist *)
let find_path (gr: label graph) (from_id: id) (to_id: id) =

  let rec find gr total_cost visited from_id to_id =
    let min_cost_path (path, cost) (other_path, other_cost) =
      if (other_path = None || cost < other_cost) then (path, cost)
      else (other_path, other_cost)
    in

    let rec loop = function
      | [] -> (None, max_int)
      | (id, (_, cost)) :: rest_arcs ->

        (* node has already been visited *)
        if List.mem id visited then loop rest_arcs
        
        (* node is unreachable *)
        else if cost = max_int then loop rest_arcs 

        (* we found the destination node! *)
        else if id == to_id then min_cost_path (loop rest_arcs) (Some(List.append visited [id]), total_cost + cost)

        else 
          (* intermediary node *)
          let (following_path, following_cost) = find gr (total_cost + cost) (List.append visited [id]) id to_id in
          if following_path = None then loop rest_arcs
          else 
            min_cost_path (following_path, following_cost) (loop rest_arcs)
    in

    let out_arcs = out_arcs gr from_id in
    loop out_arcs
  in
  find gr 0 [] from_id to_id



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
let get_min_flow_path (graph: label graph) (from_id: id) (path: path) = 

(* returns the flow value of the first arc in the path *)
let init_acu graph source first_id_in_path =
  match find_arc graph source first_id_in_path with
  | None -> raise (Path_error ("The path given doesn't exist in the graph. No arc from source #" ^ string_of_int from_id ^ " to #" ^ string_of_int first_id_in_path))
  | Some(flow, _) -> flow 
in

let rec loop graph acu from_id = function
| [] -> acu
| to_id :: rest_path ->
  match find_arc graph from_id to_id with
  | None -> raise (Path_error ("The path given doesn't exist in the graph. No arc from #" ^ string_of_int from_id ^ " to #" ^ string_of_int to_id))
  | Some(flow, _) -> loop graph (min acu flow) to_id rest_path
in
loop graph (init_acu graph from_id (List.hd path)) from_id path



let update_arcs graph from_id to_id min_flow =

  Printf.printf "from #%d to #%d\n%!" from_id to_id;
  let in_arc = find_arc graph to_id from_id in
  let out_arc = find_arc graph from_id to_id in

  (* updating the out arc *)
  match out_arc with
  | None -> raise (Graph_error ("No arc from node #" ^ string_of_int from_id ^ " to node #" ^ string_of_int to_id))
  | Some(flow, cost) -> 
       let new_flow = flow - min_flow in
       let updated_graph = 
        if new_flow = 0 then del_arc graph from_id to_id
        else new_arc graph from_id to_id (new_flow, cost) 
      in

       (* updating the in arc *)
       match in_arc with
       | None -> new_arc updated_graph to_id from_id (min_flow, (-cost))
       | Some(flow, cost) -> new_arc updated_graph to_id from_id ((flow + min_flow), cost)


(* Updates in and out arcs on the path with the update of flow value to build the new residual graph *)
let rec build_residual_graph (graph: label graph) (flow_val: int) (from_id: id) = function
  | [] -> graph
  | to_id :: rest_path ->
    let updated_arcs = update_arcs graph from_id to_id flow_val in
    build_residual_graph updated_arcs flow_val to_id rest_path


let rec busackerGowen (graph: label graph) (source: id) (sink: id) = 
  let rec get_flow = function 
  | [] -> 0
  | (id, (flow, _)) :: rest_arcs -> flow + get_flow rest_arcs
  in

  let rec loop graph source sink acu =
    let (path_option, cost) = find_path graph source sink in
    match path_option with
    | None -> (graph, acu) (* Done *)
    | Some(path) ->
      let flow_val = get_min_flow_path graph source path in
      let residual_graph = build_residual_graph graph flow_val source path in
      (*Printf.printf "path: %s\n%!" (path_to_string src path_option);*)
      loop residual_graph source sink (acu + flow_val*cost)
  in
  let (final_graph, total_cost) = loop graph source sink 0 in
  (final_graph, total_cost, get_flow (out_arcs final_graph sink))


  
(* creates a string graph from the final flow graph with max_flow/capacity (cost) labelled arcs *)
let get_final_string_graph (init_graph: label graph) (bg_graph: label graph) = 

  (* For a given flow arc, will create the flow/capacity (cost) arc on the initial graph *)
  let create_arcs init_graph from_id to_id label =
    let flow = List.hd (String.split_on_char ' ' label) in
    (* The arcs containing the flow are the ones that are in the opposite directions
       from the arcs inside of the initial graph *)
    match find_arc init_graph to_id from_id with 
    | Some(label) ->
        let label_list = String.split_on_char ' ' label in
        let capacity = List.hd label_list in
        let cost = List.nth label_list 1 in
        new_arc (init_graph) (to_id) (from_id) ("\"" ^ flow ^ "/" ^ capacity ^ " (" ^ cost ^ ")\"")
    | None -> init_graph
  in

  (* For the arcs with no flow going through them, will create a 0/capacity (cost) label *)
  let create_null_flow_arcs final_graph from_id to_id label =
    (* The arcs with no flow are the ones that haven't changed in the inital graph, they dont contain a '/' yet *)
    if String.contains_from label 0 '/' then final_graph
    else 
      let label = String.split_on_char ' ' label in
      let capacity = List.hd label in
      let cost = int_of_string (List.nth label 1) in
      new_arc (final_graph) (from_id) (to_id) ("\"0/" ^ capacity ^ " (" ^ string_of_int (-cost) ^ ")\"")
  in

  let string_bg_flow_graph = gmap bg_graph (fun (flow, cost) -> string_of_int flow ^ " " ^ string_of_int cost) in 
  let final_graph = e_fold string_bg_flow_graph create_arcs (gmap init_graph (fun (flow, cost) -> string_of_int flow ^ " " ^ string_of_int cost)) in
  e_fold final_graph create_null_flow_arcs final_graph