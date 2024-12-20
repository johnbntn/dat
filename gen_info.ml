open Core_kernel
open Poly
open Bap.Std
open Graphlib.Std
open Format
open Csv
include Self()

module CG = Graphs.Callgraph
module CFG = Graphs.Tid

(*turn unit seq into unit*)
let consume_sequence seq =
  Seq.iter ~f:(fun () -> ()) seq;
  ()

(*Create .facts file, callgraph, then add the edges of every node (function calls) to the .facts*)
let gen_callgraph prog =
  let callgraph_facts = Out_channel.create "callgraph.csv" in
  let cg = Program.to_graph prog in
  CG.edges cg |> Seq.map ~f:(fun edge ->
      let src = Tid.name (CG.Edge.src edge) in
      let dst = Tid.name (CG.Edge.dst edge) in
      let call = [src; dst] in
      Csv.output_record (Csv.to_channel callgraph_facts) call)
      

let main proj =
  let prog = Project.program proj in
  consume_sequence (gen_callgraph prog)
  

module Cmdline = struct
  open Config

  let () = when_ready (fun _ ->
      Project.register_pass' (main))

  let () = manpage [
      `S "DESCRIPTION";
      `P
      "Generates call graph and control flow graph information
      about a binary and formats it into csv files to perform
      binary analysis in Datalog"
    ]
end


    
