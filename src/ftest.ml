open Gfile
open Tools
open Fordfulkerson
open Mbp

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  in

  (* Open file *)
  let graph = clean_graph (from_mbp_file infile) in
  let (source_id, sink_id, graph1) = add_source_and_sink graph in

  (* Rewrite the graph that has been read and potentially modified. *)
  let () = export outfile (gmap graph1 string_of_int) in
  ()

