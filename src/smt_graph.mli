(** Graph of formulas

    Each nodes is a conjunction (list) of boolean formulas with an id (for uniqueness purposes).

    The implementation is functorized to hide Z3's context. Here is the typical way to use this module :
    {[
      module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
      module Smtg = Smt_graph.Make (ZZ3)
    ]}
*)

module Make (ZZ3 : ZZ3_sigs.S) : sig

  type vertex_ = {
    id : int ;
    formulas : ZZ3.zbool ZZ3.t list ;
  }

  include Graph.Sig.P
    with type V.t = vertex_
     and type V.label = vertex_
     and type E.label = unit


  (** {2 Dot export} *)

  (** Pretty print to dot graphviz format.*)
  module Dot : sig
    val fprint_graph : Format.formatter -> t -> unit
    val output_graph : Pervasives.out_channel -> t -> unit
  end


  (** {2 Transformation to SMT formula} *)

  (** Gather all the formulas of the graph in one big smt formula. *)
  val to_smt : t -> [> ZZ3.zbool ] ZZ3.t

end
