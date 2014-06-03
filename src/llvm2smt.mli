(** Transform an llvm graph into an smt formula.

    The implementation is functorized to hide Z3's context. Here is the typical way to use this module :
    {[
      module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
      module Smtg = Smt_graph.Make (ZZ3)
      module L2S = Llvm2smt.Init (ZZ3) (Smtg)
    ]}
*)

module Init
    (ZZ3 : ZZ3_sigs.S)
    (SMTg : module type of Smt_graph.Make(ZZ3))
  : sig

    (** Environment mapping llvm values (primed, or not) to smt values.
        A phi variable is primed if it's used from a split node.
    *)
    val env : (Llvm.llvalue * bool option, Z3.Expr.expr) Hashtbl.t

    (** Transform and Llvm graph into a formula graph, feeling {! env} on the way. *)
    val llvm2smt : Llvm_graph.t -> SMTg.t

  end
