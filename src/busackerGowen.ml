open Graph

type label = int * int
type path = id list


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
