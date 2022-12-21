open Graph

(* the label on an arc is the flow and the cost of the arc *)
type label = int * int
type path = id list

(*val find_path: label graph -> id -> id -> path option*)