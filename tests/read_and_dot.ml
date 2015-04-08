(**
   This program takes 3 file name in command line and
   will put in them respectively
   - the normal llvm control flow graph of the first function.
   - The llvm cfg once broken into a dag.
   - the graph with the smt encoding of each node.
*)

open Llvm
module Llg = Llvmcfg

(* Little functor instanciation gymnastic to get everyone on board. *)
module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
module SMTg = Smtgraph.Make (ZZ3)
module L2S = Llvm2smt.Init (ZZ3) (SMTg)

let () =

  (* We start by some llvm boiler plate to read a file and apply mem2reg. *)
  let ctx = Llvm.create_context () in

  let mem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let m = Llvm_bitreader.parse_bitcode ctx mem in
  Llvm.MemoryBuffer.dispose mem ;

  let pass = PassManager.create () in
  Llvm_scalar_opts.add_memory_to_register_promotion pass ;
  ignore @@ PassManager.run_module m pass ;

  (* We read the first function. *)
  let llf = match function_begin m with
    | At_end _ -> Printf.eprintf "There is no function!\n%!" ; exit 1
    | Before v -> v
  in
  let llb2node, llg = Llg.of_llfunction llf in

  (* Export the dot of the cfg to the first file. *)
  let chout = open_out Sys.argv.(2) in
  Llg.Dot.output_graph chout llg ;
  close_out chout ;

  (* Break the graph into a DAG and export it to the second file. *)
  let (points, llg') = Llg.break_graph llg in

  let chout = open_out Sys.argv.(3) in
  Llg.Dot.output_graph chout llg' ;
  close_out chout ;

  (* Apply the smt encoding and export into the third file. *)
  let smtg = L2S.llvm2smt llf points llg' in

  let chout = open_out Sys.argv.(4) in
  SMTg.Dot.output_graph chout smtg ;
  close_out chout ;

  (* Finally, print the formula. *)
  let smt = (SMTg.to_smt smtg : [> ZZ3.zbool ] ZZ3.term :> Z3.Expr.expr) in
  print_endline @@ Z3.Expr.to_string smt ;
