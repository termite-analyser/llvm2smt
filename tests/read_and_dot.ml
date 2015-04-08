open Llvm
open Llvmcfg

module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
module SMTg = Smtgraph.Make (ZZ3)
module Llvm2Smt = Llvm2smt.Init (ZZ3) (SMTg)
open Llvm2Smt

let () =
  let ctx = Llvm.create_context () in


  let mem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let m = Llvm_bitreader.parse_bitcode ctx mem in
  Llvm.MemoryBuffer.dispose mem ;

  let pass = PassManager.create () in
  Llvm_scalar_opts.add_memory_to_register_promotion pass ;
  ignore @@ PassManager.run_module m pass ;

  let Before llf = function_begin m in
  let llb2node, llg = of_llfunction llf in

  let chout = open_out Sys.argv.(2) in
  Dot.output_graph chout llg ;
  close_out chout ;


  let (points, llg') = break_graph llg in

  let chout = open_out Sys.argv.(3) in
  Dot.output_graph chout llg' ;
  close_out chout ;


  let smtg = llvm2smt llf points llg' in

  let chout = open_out Sys.argv.(4) in
  SMTg.Dot.output_graph chout smtg ;
  close_out chout ;

  let smt = (SMTg.to_smt smtg : [> ZZ3.zbool ] ZZ3.term :> Z3.Expr.expr) in
  print_endline @@ Z3.Expr.to_string smt ;
