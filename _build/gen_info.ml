open Core_kernel
open Poly
open Bap.Std
open Graphlib.Std
open Format
open Csv
include Self()

module CG = Graphs.Callgraph
module CFG = Graphs.Tid

let create_facts oc fact =
  fact |> List.iter ~f:(fun fact ->
      Out_channel.output_string oc (String.concat ~sep:"\t" fact);
      Out_channel.output_char oc '\n')

(*turn unit seq into unit*)
let consume_sequence seq =
  Seq.iter ~f:(fun () -> ()) seq;
  ()

(*Create .facts file, callgraph, then add the edges of every node (function calls) to the .facts*)
let gen_callgraph prog =
  let callgraph_facts = Out_channel.create "callgraph.facts" in
  let cg = Program.to_graph prog in
  CG.edges cg |> Seq.iter ~f:(fun edge ->
      let src = (Tid.name (CG.Edge.src edge)) in
      let dst = (Tid.name (CG.Edge.dst edge)) in
      let call = [src; dst] in
      create_facts callgraph_facts [call]);
  Out_channel.close callgraph_facts

(*
let gen_cfg prog =
  let cfg_facts = Out_channel.create "cfg.facts" in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter ~f:(fun blk ->
          Term.enum jmp_t blk |> Seq.iter ~f:(fun j ->
              match Jmp.kind j with
              | Goto _ | Ret _ | Int(_,_) -> ()
              | Call dst -> match Call.target dst with
                | Direct tid ->
                  let src = Sub.to_string sub in
                  let dst = Call.to_string dst in
                  let call = [src; dst] in
                  create_facts cfg_facts [call];
                | _ -> ())));
  Out_channel.close cfg_facts
      
*)

let main proj =
  let prog = Project.program proj in
  gen_callgraph prog;
  (*gen_cfg prog;*)
  

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


    
