open Graph
open Utils

type vertex_ = {
  id : int ;
  block : Llvm.llbasicblock ;
  phi : Llvm.llvalue list ;
  instr : Llvm.llvalue list ;
}

let vertex =
  let r = ref 0 in
  fun ~block ~phi ~instr ->
    incr r ;
    { id = !r ; block ; phi ; instr }

let llblock2vertex llb =
  let aux (phi, instr) llv =
    match Llvm.instr_opcode llv with
      | Llvm.Opcode.PHI -> (llv :: phi, instr)
      | _ -> (phi, llv :: instr)
  in
  let (phi, instr) = Llvm.fold_left_instrs aux ([],[]) llb in
  vertex ~block:llb ~phi:(List.rev phi) ~instr:(List.rev instr)


module V_ = struct
  type t = vertex_
  let compare x y = compare x.id y.id
  let hash x = Hashtbl.hash x.id
  let equal x y = x.id = y.id
end

module E_ = struct
  type t = Llvm.lluse option
  let compare = compare
  let default = None
end

module Llg = Persistent.Digraph.ConcreteBidirectionalLabeled (V_) (E_)
include Llg

module B = Llvmgraph.Map(Builder.P(Llg))

(** Create the graph representing an llvm function.

    Returns a couple [(f, g)] where [g] is the graph and [f] associate
    an llvm basic bloc to a node in the graph.
*)
let of_llfunction llfun =
  B.map
    ~vertex:llblock2vertex
    ~label:(fun x -> Some (Llvmgraph.G.E.label x))
    llfun


(** Graphviz is cool. *)
module Dot = Graphviz.Dot (struct
    include Llg

    let graph_attributes _ = []
    let default_vertex_attributes _ = []

    let vertex_name { id ; block ; phi ; instr } =
      let instrs =
        String.concat "\n" @@
        List.map Llvm.string_of_llvalue (phi @ instr)
      in
      Printf.sprintf "\"block:%i\\n%s\"" id instrs
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []

  end)



(** Split a node in two smaller nodes:
    One with all the phi, the other will all the instructions.
    Previous input edges go to the phi node.
    Previous output edges leave from the instruction node.
*)
let break_node g ({ block ; phi ; instr } as node) =
  (* In and out edges. *)
  let in_e = Llg.pred_e g node in
  let out_e = Llg.succ_e g node in

  (* The two new nodes. *)
  let node_phi = vertex ~block ~phi ~instr:[] in
  let node_instr = vertex ~block ~phi:[] ~instr in

  g
  (* First, we remove the old edges and the node. *)
  |> flip (List.fold_left Llg.remove_edge_e) in_e
  |> flip (List.fold_left Llg.remove_edge_e) out_e
  |> flip Llg.remove_vertex node

  (* We add the new nodes. *)
  |> flip Llg.add_vertex node_phi
  |> flip Llg.add_vertex node_instr

  (* And then we add back the edges, slightly modified. *)
  (* We take care of handling the case of a self-loop. *)
  |> fun g -> List.fold_left (fun g (src,l,_) ->
      if V.equal src node
      then Llg.add_edge_e g (node_instr,l,node_phi)
      else Llg.add_edge_e g (src,l,node_phi)
    ) g in_e
  |> fun g -> List.fold_left (fun g (_,l,dst) ->
      if V.equal dst node
      then Llg.add_edge_e g (node_instr,l,node_phi)
      else Llg.add_edge_e g (node_instr,l,dst)
    ) g out_e


let basicblocks_to_vertices g control_points =
  let h = Hashtbl.create 16 in
  Llg.iter_vertex (fun v -> Hashtbl.add h v.block v) g ;
  let l =
    try List.map (Hashtbl.find h) control_points
    with Not_found -> raise (Invalid_argument "Break_list: No such basicblock in the graph")
  in
  l

let break_by_list g l =
  List.fold_left break_node g l


module Cutset = Mincut.Make (Llg)
module Choose = Oper.Choose (Llg)

exception Not_reducible of t

(** Retrieve the minimal vertex cut set of a graph,
    and apply {!break_node} on each of these vertexes.
*)
let break_graph ?start g =
  let start = match start with
    | None -> Choose.choose_vertex g
    | Some n -> n
  in
  try
    let l = Cutset.min_cutset g start in
    let control_points = List.map (fun x -> x.block) l in
    control_points, break_by_list g l
  with
    | Invalid_argument "Graph.Mincut: graph not reducible" ->
      raise (Not_reducible g)
