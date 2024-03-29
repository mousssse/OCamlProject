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
  let (from_title, to_title, init_graph) = from_mbp_file infile in
  let (mbp_graph, mbp_val) = mbp init_graph in
  Printf.printf "Maximum number of %s(s) that can get a %s is %d\n" from_title to_title mbp_val;

  (* Rewrite the graph that has been read and potentially modified. *)
  let () = export outfile mbp_graph in
  ()

