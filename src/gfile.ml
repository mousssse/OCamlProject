open Graph
open Printf

type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used).
   n 88.8 209.7
   n 408.9 183.0

   % The first node has id 0, the next is 1, and so on.

   % Edges: e source dest label
   e 3 1 11
   e 0 2 8

*)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %.1f 1.0\n" (float_of_int id)) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff "e %d %d %s\n" id1 id2 lbl) ;

  fprintf ff "\n%% End of graph\n" ;

  close_out ff ;
  ()

let export path graph = 
  let dot_file = open_out path in

  fprintf dot_file "/* This is a dot format graph. */\n";
  fprintf dot_file "digraph finite_state_machine {\n	rankdir=LR;\n    ";

  n_iter graph (fun id -> fprintf dot_file "%d " id);
  fprintf dot_file ";\n";
  e_iter graph (fun id1 id2 cost -> fprintf dot_file "    %d -> %d [label = %s];\n" id1 id2 cost);

  fprintf dot_file "}";
  close_out dot_file ;
  ()

(* Reads a line with a node. *)
let read_node id graph line =
  try Scanf.sscanf line "n %f %f" (fun _ _ -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %s"
        (fun id1 id2 label -> new_arc (ensure (ensure graph id1) id2) id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph)

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> (n+1, read_node n graph line)
          | 'e' -> (n, read_arc graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)
      in      
      loop n2 graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 0 empty_graph in

  close_in infile ;
  final_graph


(* Format of mbp text files:
   %% This is a comment

   %% From/To node names
    1 applicant %this is the name of the "from nodes"
    2 job       %this is the name of the "to nodes"

   % "to" nodes
   t
   t

   % The first node has id 0, the next is 1, and so on.

   % "from" nodes: f ["to nodes" list]
   f [1 2]    %there is an arc from f to 1 and from f to 2
   f []       %there are no arcs from f

*)



(* Reads a line with a node. *)
let read_to_node id graph line =
  try Scanf.sscanf line "t" (new_node graph id)
  with e ->
    Printf.printf "Cannot read 'to node' in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let rec create_arcs id graph to_nodes = 
  let node_list = List.map int_of_string (String.split_on_char ' ' to_nodes) in

  let rec loop id graph = function
    | [] -> graph
    | to_id :: rest_nodes ->
      let graph = new_arc graph id to_id 1 in
      loop id graph rest_nodes
  in

  loop id graph node_list

(* Reads a line with an arc. *)
let read_from_node id graph line =
  try 
    let ind1 = (String.index_from line 0 '[')+1 in
    let ind2 = String.index_from line 0 ']' in
    (* if no out arcs: just adding the node *)
    (* TODO: think abt from/to nodes that connect to no one, we don't need them... *)
    if ind2 = ind1 then new_node graph id
    else
      let to_nodes = String.sub line (ind1) (ind2-ind1) in
      Printf.printf "to_nodes: [%s]\n" to_nodes; (* To debug *)
      let graph = new_node graph id in
      create_arcs id graph to_nodes
  with e -> 
    Printf.printf "Cannot read 'from node' in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let from_mbp_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph)

        (* The first character of a line determines its content : t or f. *)
        else match line.[0] with
          | '1' -> (n, graph) (* Ignore title line of "from nodes" *)
          | '2' -> (n, graph) (* Ignore title line of "to nodes" *)
          | 't' -> (n+1, read_to_node n graph line)
          | 'f' -> (n+1, read_from_node n graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)
      in      
      loop n2 graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 0 empty_graph in

  close_in infile ;
  final_graph