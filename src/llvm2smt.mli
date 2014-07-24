(** Transform an llvm graph into an smt formula.

    The implementation is functorized to hide Z3's context. Here is the typical way to use this module :
    {[
      module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
      module Smtg = Smt_graph.Make (ZZ3)
      module L2S = Llvm2smt.Init (ZZ3) (Smtg)
    ]}
*)

exception Not_implemented of Llvm.llvalue
val sprint_exn : Llvm.llvalue -> string

exception Variable_not_found of (bool * Llvm.llvalue)
val sprint_exn_var : bool * Llvm.llvalue -> string

exception Block_not_found of (bool * Llvm.llbasicblock)
val sprint_exn_block : bool * Llvm.llbasicblock -> string

module Init
    (ZZ3 : ZZ3_sigs.S)
    (SMTg : module type of Smt_graph.Make(ZZ3))
  : sig

    (** Environment mapping llvm values (primed, or not) to smt values.
        A phi variable is primed if it's used from a split node.
    *)
    val env : (Llvm.llvalue * bool, Z3.Expr.expr) Hashtbl.t

    val get_var : bool -> Llvm.llvalue -> [> ZZ3.znum ] ZZ3.term

    val get_block : bool -> Llvm.llbasicblock -> [> ZZ3.zbool ] ZZ3.term

    (** Transform an Llvm graph into a formula graph, filling {! env} on the way. *)
    val llvm2smt : Llvm.llvalue -> Llvm.llbasicblock list -> Llvm_graph.t -> SMTg.t

  end
