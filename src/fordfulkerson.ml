open Graph
open Tools
open Printf

type path = id list

let path_to_file outfile path =
  let rec loop = function
    | [] -> ""
    | id :: [] -> string_of_int id
    | id :: rest -> (string_of_int id) ^ " -> " ^ (loop rest)
  in

  (* Open a write-file. *)
  let ff = open_out outfile in

  (* Write in this file. *)
  fprintf ff "%% This is a path.\n\n" ;

  let path_string = loop path in
  fprintf ff "%s" path_string ;

  fprintf ff "\n%% End of path\n" ;

  close_out ff ;
  ()

let find_path (gr: int graph) (forbidden_nodes: id list) (from_id: id) (to_id: id) =
  let rec path_without_first gr forbidden_nodes from_id to_id =
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
          let following_path = path_without_first gr (List.append forbidden_nodes [id] ) id to_id in
          if following_path = None then loop rest_arcs else following_path
    in
    
    let out_arcs = out_arcs gr from_id in
    loop out_arcs;
  in

  match path_without_first gr forbidden_nodes from_id to_id with
  | Some(path) -> Some(from_id :: path)
  | None -> None