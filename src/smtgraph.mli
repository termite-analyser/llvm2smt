(** Graph of formulas

    Each nodes is a conjunction (list) of boolean formulas with an id (for uniqueness purposes).

    The implementation is functorized to hide Z3's context. Here is the typical way to use this module :
    {[
      module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
      module Smtg = Smtgraph.Make (ZZ3)
    ]}
*)

module Make (ZZ3 : ZZ3_sigs.S) : sig


  include Graph.Sig.P
     with type E.label = unit

  val get_clauses : V.t -> ZZ3.zbool ZZ3.term list

  val from_llvm :
    (Llvmcfg.vertex -> ZZ3.zbool ZZ3.term list) ->
    Llvmcfg.vertex -> vertex

  (** {2 Dot export} *)

  (** Pretty print to dot graphviz format.*)
  module Dot : sig
    val fprint_graph : Format.formatter -> t -> unit
    val output_graph : Pervasives.out_channel -> t -> unit
  end


  (** {2 Transformation to SMT formula} *)

  (** Gather all the formulas of the graph in one big smt formula. *)
  val to_smt : t -> [> ZZ3.zbool ] ZZ3.term

end
