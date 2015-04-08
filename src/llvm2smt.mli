(** Transform an llvm graph into an smt formula.

    The implementation is functorized to hide Z3's context. Here is the typical way to use this module :
    {[
      module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
      module Smtg = Smtgraph.Make (ZZ3)
      module L2S = Llvm2smt.Init (ZZ3) (Smtg)
    ]}

    Then, assuming you have [llf] an llvm function:
    {[
      let smt =
        let llb2node, llg = Llvmcfg.of_llfunction llf in
        let breakpoints, llg' = Llvmcfg.break_graph llg in
        let smtg = L2S.llvm2smt llf points llg' in
        SMTg.to_smt smtg
    ]}

    Note that this functor *is* stateful. It will build up the environment [env] as it translate functions which allows to find the smt variable from an llvm variable.
*)

(** Raised if an llvm instruction is not implemented. *)
exception Not_implemented of Llvm.llvalue
val sprint_exn : Llvm.llvalue -> string

(** Raised if an llvm variable is used in the control flow graph but doesn't exist. *)
exception Variable_not_found of (bool * Llvm.llvalue)
val sprint_exn_var : bool * Llvm.llvalue -> string

(** Raised if an llvm basic bloc is used in the control flow graph but doesn't exist. *)
exception Block_not_found of (bool * Llvm.llbasicblock)
val sprint_exn_block : bool * Llvm.llbasicblock -> string

module Init
    (ZZ3 : ZZ3_sigs.S)
    (SMTg : module type of Smtgraph.Make(ZZ3))
  : sig

    (** Environment mapping llvm values (primed, or not) to smt values.
        A phi variable is primed if it's used from a split node.
    *)
    val env : (Llvm.llvalue * bool, Z3.Expr.expr) Hashtbl.t

    val get_var : bool -> Llvm.llvalue -> [> ZZ3.znum ] ZZ3.term

    val get_block : bool -> Llvm.llbasicblock -> [> ZZ3.zbool ] ZZ3.term

    (** Transform an Llvm graph into a formula graph, filling {! env} on the way. *)
    val llvm2smt : Llvm.llvalue -> Llvm.llbasicblock list -> Llvmcfg.t -> SMTg.t

  end
