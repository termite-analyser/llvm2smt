
open Graph

module Make (ZZ3 : ZZ3_sigs.S) = struct
  open ZZ3

  type vertex_ = {
    id : int ;
    formulas : zbool t list ;
  }

  module SMTg = Persistent.Digraph.ConcreteBidirectional
      (struct
        type t = vertex_
        let compare = compare
        let hash = Hashtbl.hash
        let equal = (==)
      end)
  include SMTg

  let to_smt g =
    T.and_ (SMTg.fold_vertex (fun x l -> T.and_ x.formulas :: l) g [])


  (** Graphviz is cool. *)
  module Dot = Graphviz.Dot (struct
      include SMTg

      let graph_attributes _ = []
      let default_vertex_attributes _ = []

      let vertex_name { id ; formulas } =
        let instrs =
        String.concat "\n" @@
        List.map Z3.Expr.to_string (formulas :> Z3.Expr.expr list)
        in
        Printf.sprintf "\"block:%i\\n%s\"" id instrs
      let vertex_attributes _ = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []

    end)


end
