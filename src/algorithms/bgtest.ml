open Gfile
open Tools
open BusackerGowen

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
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_bg_file infile in

  (*let (path_opt, cost) = find_path graph source sink in
  let path = (Option.get path_opt) in
  let min_flow = get_min_flow_path graph source path in
  Printf.printf "path: %s. cost: %d. min flow: %d.\n%!"
    (path_to_string source path_opt) 
    cost 
    (min_flow)
  ;

  let residual_graph = build_residual_graph graph min_flow source path in*)

  let (final_graph, total_cost, max_flow) = busackerGowen graph source sink in
  let string_graph = get_final_string_graph graph final_graph in
  Printf.printf "The solution has a max flow of %d with a min cost of %d" max_flow total_cost;

  (* Rewrite the graph that has been read and potentially modified. *)
  let () = export outfile string_graph in
  ()

