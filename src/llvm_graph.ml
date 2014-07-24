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
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module E_ = struct
  type t = Llvm.lluse option
  let compare = compare
  let default = None
end

module Llg = Persistent.Digraph.ConcreteBidirectionalLabeled (V_) (E_)
include Llg


let is_terminator llv =
  let open Llvm.ValueKind in
  let open Llvm.Opcode in
  match Llvm.classify_value llv with
    | Instruction (Br | IndirectBr | Invoke | Resume | Ret | Switch | Unreachable)
        -> true
    | _ -> false

let has_successor llb =
  match Llvm.block_terminator llb with
    | Some lli -> begin
        let open Llvm.Opcode in
        match Llvm.instr_opcode lli with
          | Ret | Unreachable -> false
          | _ -> true
      end
    | _ -> false

let predecessors llb =
  let llv = Llvm.value_of_block llb in
  let aux l llu =
    let lli = Llvm.user llu in
    let llb' = Llvm.instr_parent lli in
    if is_terminator lli
    then (llb', llu) :: l
    else l
  in
  Llvm.fold_left_uses aux [] llv

(** Create the graph representing an llvm function.

    Returns a couple [(f, g)] where [g] is the graph and [f] associate
    an llvm basic bloc to a node in the graph.
*)
let of_llfunction llfun =
  let h = Hashtbl.create 128 in
  let f_add_vertex g llb =
    let v = llblock2vertex llb in
    Hashtbl.add h llb v ;
    add_vertex g v
  in
  let g = Llvm.fold_left_blocks f_add_vertex empty llfun in
  let f_add_edges node g =
    let pred = predecessors node.block in
    let aux g (llb', llu) =
      add_edge_e g (Hashtbl.find h llb', Some llu, node)
    in
    List.fold_left aux g pred
  in
  Hashtbl.find h, fold_vertex f_add_edges g g


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

let break_list g l =
  List.fold_left break_node g l

(*
module Cutset = Cutset.Make (Llg)

exception Not_reducible of t

(** Retrieve the minimal vertex cut set of a graph,
    and apply {!break_node} on each of these vertexes.
*)
let break_scc g start =
  match Cutset.min_cutset g start with
    | None -> raise (Not_reducible g)
    | Some l ->
        let control_points = List.map (fun x -> x.block) l in
        control_points, break_list g l
*)
