open Gfile
open Tools
open Fordfulkerson

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (*let clone_nodes_test = clone_nodes graph in*)
  (*let gmap_graph = gmap graph (fun x -> x^"!") in*)

  let int_graph = gmap graph int_of_string in
    (*let graph_plus_100 = gmap (add_arc int_graph 3 1 100) string_of_int in*)
    (*let graph_plus_100 = gmap (add_arc int_graph 2 5 100) string_of_int in*)


  (*let path_test = find_path int_graph [] source sink in
  Printf.printf "path: %s\n%!" (path_to_string source path_test);

  let path = Option.get path_test in
  let flow_val = get_min_flow_path int_graph 1000 source path in
  let residual_graph = build_residual_graph int_graph flow_val source path in*)

  let final_graph = fordfulkerson int_graph source sink in

  (* Rewrite the graph that has been read and potentially modified. *)
  let () = export outfile (gmap final_graph string_of_int) in
  ()

