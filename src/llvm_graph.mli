(** Ocamlgraph persistent overlay for llvm.

    This module allows to extract and manipulate a graph representing an llvm function with nodes representing the basicblocks.

    The graph is persistent so that it's possible to transform the representation to perform analysis without modifying Llvm's IR.
*)

type vertex_ = {
  id : int ;
  block : Llvm.llbasicblock ;
  phi : Llvm.llvalue list ;
  instr : Llvm.llvalue list ;
}

include Graph.Sig.P
  with type V.t = vertex_
   and type V.label = vertex_
   and type E.label = Llvm.lluse option

(** Create the graph representing an llvm function.

    Returns a couple [(f, g)] where [g] is the graph and [f] associate
    an llvm basic bloc to a node in the graph.
*)
val of_llfunction : Llvm.llvalue -> (Llvm.llbasicblock -> vertex) * t


(** {2 Misc functions} *)

(** Return true iff a instruction is a terminator. *)
val is_terminator : Llvm.llvalue -> bool

(** Return true iff a block has a successor. *)
val has_successor : Llvm.llbasicblock -> bool

(** Return the list of predecessors (and the associated user) of a block. *)
val predecessors : Llvm.llbasicblock -> (Llvm.llbasicblock * Llvm.lluse) list

(** {2 Dot export} *)

(** Pretty print to dot graphviz format.*)
module Dot : sig
  val fprint_graph : Format.formatter -> t -> unit
  val output_graph : Pervasives.out_channel -> t -> unit
end


(** {2 Cycle breaking} *)

(** Get the vertices associated to some basicblocks. *)
val basicblocks_to_vertices : t -> Llvm.llbasicblock list -> vertex list

(** Break the node of a graph in two parts :
    - A phi node which contains only the phis and receives the input edges.
    - An instruction node which contains only the instruction and emit the output edges.
*)
val break_list : t -> vertex list -> t


(* This code use a custom version of ocamlgraph with Shamir's algorithm.
   See https://github.com/Drup/ocamlgraph/blob/cutset/src/cutset.mli
*)
(*
exception Not_reducible of t

(** Break all the cycle in a graph, effectively returning a DAG.
    Also returns the list of broken basicblocks.

    A graph must be reducible for the algorithm to apply (which should be the case for all llvm's graphs) and the function will raise {! Not_reducible} otherwise.
*)
val break_scc : t -> vertex -> Llvm.llbasicblock list * t
*)
