open Core_kernel
open Poly
open Bap.Std
open Graphlib.Std
open Format
include Self()

module CG = Graphs.Callgraph
module CFG = Graphs.Tid

open Core_kernel
open Bap.Std

(* note, this function will return all variables, including
   non-free *)
let vars_of_exp = Exp.fold ~init:Var.Set.empty (object
    inherit [Var.Set.t] Exp.visitor
    method! enter_var var vars = Set.add vars var
  end)

let vars_of_label = function
  | Indirect exp -> vars_of_exp exp
  | Direct _ -> Var.Set.empty

let collect_vars sub =
  let (++) = Set.union in
  Term.enum blk_t sub |>
  Seq.fold ~init:(Var.Set.empty,Var.Set.empty) ~f:(fun sets blk ->
      Blk.elts blk |> Seq.fold ~init:sets ~f:(fun (defs,uses) ->
          function
          | `Phi phi ->
            Set.add defs (Phi.lhs phi),
            Seq.fold (Phi.values phi) ~init:uses ~f:(fun uses (_,exp) ->
                uses ++ vars_of_exp exp)
          | `Def def ->
            Set.add defs (Def.lhs def),
            uses ++ vars_of_exp (Def.rhs def)
          | `Jmp jmp ->
            defs,
            uses ++ vars_of_exp (Jmp.cond jmp) ++
            match Jmp.kind jmp with
            | Ret dst | Goto dst -> vars_of_label dst
            | Int (_,_) -> Var.Set.empty
            | Call call ->
              uses ++ vars_of_label (Call.target call) ++
              match Call.return call with
              | None -> Var.Set.empty
              | Some dst -> vars_of_label dst))

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
  let callgraph_facts = Out_channel.create "facts/callgraph.facts" in
  let cg = Program.to_graph prog in
  CG.edges cg |> Seq.iter ~f:(fun edge ->
      let src = (Tid.name (CG.Edge.src edge)) in
      let dst = (Tid.name (CG.Edge.dst edge)) in
      let call = [src; dst] in
      create_facts callgraph_facts [call]);
  Out_channel.close callgraph_facts

let gen_ssa prog arch =
  let module Target = (val target_of_arch arch) in
  let no_side_effects var =
    let open Target.CPU in
    Var.is_virtual var || is_flag var in
  let uses_facts = Out_channel.create "facts/uses.facts" in
  let defs_facts = Out_channel.create "facts/defs.facts" in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      let defs_init,uses_init = collect_vars sub in
      let defs = Set.filter defs_init ~f:no_side_effects in
      let uses = Set.filter uses_init ~f:no_side_effects in
      let defs_list = Var.Set.to_sequence defs |> Seq.to_list |> List.map ~f:(fun def ->
          Var.name def) in
      let uses_list = Var.Set.to_sequence uses |> Seq.to_list |> List.map ~f:(fun use ->
          Var.name use) in

      List.iter defs_list ~f:(fun def -> create_facts defs_facts [[def]]);
      List.iter uses_list ~f:(fun use -> create_facts uses_facts [[use]]));
  Out_channel.close defs_facts;
  Out_channel.close uses_facts
      
  
let main proj =
  let prog = Project.program proj in
  gen_callgraph prog;
  gen_ssa prog (Project.arch proj);
  

module Cmdline = struct
  open Config

  let () = when_ready (fun _ ->
      Project.register_pass' ~deps:["ssa"] (main))

  let () = manpage [
      `S "DESCRIPTION";
      `P
      "Generates information
      about a binary and formats it into .facts files to perform
      binary analysis in Souffle Datalog"
    ]
end


    
