# Llvm2smt

Encode an llvm control flow graph into an smt formula, producing a mapping from llv variables to smt variables on the way.
ℕ is used to represent integers and ℚ to represent floating point numbers. Bit by bit operations are not supported at the moment, as well as memory accesses and exceptions.

Dependencies are:
- ocamlgraph
- Microsoft's Z3 and the ml-ng bindings.
- [Z3overlay](https://github.com/Drup/ocaml-z3)
- llvm >= 3.6
- [llvmgraph](https://github.com/Drup/llvmgraph) >= 0.2

You can see an example in the file [`read_and_dot.ml`](tests/read_and_dot.ml). The library is used like that:

```ocaml
(* First, instanciate some functors. *)
module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
module Smtg = Smtgraph.Make (ZZ3)
module L2S = Llvm2smt.Init (ZZ3) (Smtg)

(* Then, assuming you have an llvm function llf *)
let smt =
  let llb2node, llg = Llvmcfg.of_llfunction llf in
  let breakpoints, llg' = Llvmcfg.break_graph llg in
  let smtg = L2S.llvm2smt llf points llg' in
  SMTg.to_smt smtg
```

This proceeds by the following steps. We will illustrate with the algorithm applied on the example [`example.c`](tests/example.c):
- Builds a persistent ocamlgraph representation of the control flow graph using [llvmgraph](https://github.com/Drup/llvmgraph)'s Map functor.
  ![Llvm control flow graph](https://i.imgur.com/ijRiaE5.png)
- Breaks the cycle in this graph by computing a minimal vertex cutset of it and breaking each selected basicblock into a block with phi nodes and a block with the rest. These node corresponds to the important control points of the program (loop headers, for example).
  ![Llvm broken control flow graph](https://i.imgur.com/ptaYaG0.png)
- Build a graph of the same shape containing lists of clauses. Each clauses is build by encoding an llvm instruction into an smt term. Note that the encoding differanciate variables in vertexes that were splited and contains only phi node. We call these variables "primed" as we want to distinguish them from their counterpart in the other node.
  ![Smt graph](https://i.imgur.com/lo3roz9.png)
- The final formula is produced by taking the and of all the clauses of all the basic block.
```
(and (= _28 (< .0_20 y_2))
     (= e_17 (and _28 b_10))
     (= e_18 (and (not _28) b_10))
     (= (or e_16 e_11) b_prime_9)
     (= .0_prime_27 (ite e_11 x_1 (ite e_16 .1_24 phi_else_26)))
     (= (or e_18) b_8)
     (= (or e_15 e_14) b_7)
     (= .1_24 (ite e_14 _21 (ite e_15 _22 phi_else_23)))
     (= b_7 e_16)
     (= (or e_13) b_6)
     (= _22 (+ .0_20 2))
     (= b_6 e_15)
     (= (or e_12) b_5)
     (= _21 (+ .0_20 1))
     (= b_5 e_14)
     (= (or e_17) b_4)
     (= _19 (< .0_20 3))
     (= e_12 (and _19 b_4))
     (= e_13 (and (not _19) b_4))
     (not b_3)
     (= b_3 e_11))
```

Once all that is done, if you want to find out if a property hold for executions along some loop path
- You find the loop header, using the `breakpoints` list.
- You find the two smt variables corresponding to this loop header, using `L2S.get_basicblock` (with the `prime` argument once true and once false)
- You create a formula fixing theses at true.
- You encode your property
- You ask Z3 to solve that.

If it holds, it's SAT, otherwise, you get a counter example. Beware that not the whole program is encoded here, not all properties can be proven like that, and you may need extra information, like invariants. :)
