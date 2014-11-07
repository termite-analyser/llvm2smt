open Utils
open ZZ3
open Llvm

module Llg = Llvm_graph

exception Not_implemented of llvalue
let sprint_exn llv =
  Printf.sprintf "Llvm2smt, Instruction not implemented: %s"
    (string_of_llvalue llv)
let () =
  Printexc.register_printer (function
    | Not_implemented llv -> Some (sprint_exn llv)
    | _ -> None)

exception Variable_not_found of (bool * llvalue)
let sprint_exn_var (primed, llv) =
  Printf.sprintf "Could not find the value %s with primed: %B\n%!"
    (value_name llv) primed

exception Block_not_found of (bool * llbasicblock)
let sprint_exn_block (primed, llb) =
  Printf.sprintf "Could not find the following block with primed: %B.\n%s%!"
    primed (value_name @@ value_of_block llb)


module H = Hashtbl.Make (Llg.V)


module Init (ZZ3 : ZZ3_sigs.S) (SMTg : module type of Smt_graph.Make(ZZ3))= struct

  open ZZ3

  (** Existential type used to work around some type issues with SMT's gadt interface. *)
  (* Don't use if you can. *)
  type ex = Ex : (_,[< zany] as 'a) typ -> ex

  (** Create a name of the form "$x[_primed]_$i".
      - $x the original name of variable.
      - $i a unique number.
  *)
  let make_name =
    let counter = ref 0 in
    fun ?(primed=false) s ->
      let prime =
        if primed then "_prime" else ""
      in
      incr counter ;
      Printf.sprintf "%s%s_%i" s prime !counter

  (** true iff the phi variable inside this block should be primed. *)
  let is_primed_block {Llg. phi ; instr } = phi <> [] && instr = []


  (** Variable access and creation. *)

  let env : (_, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 512

  (** Some access only function, to be exposed in the interface *)


  let get_var primed llv =
    try
      let e = Hashtbl.find env (llv,primed) in
       (** Fix z3's retardedness by preemptively upcasting to a real. *)
      let e' = Z3.Arithmetic.Integer.mk_int2real ZZ3.ctx e in
      ZZ3.(T.symbol (Symbol.trustme Num e'))
    with Not_found -> raise @@ Variable_not_found (primed, llv)


  let get_block primed llb =
    try
      let e = Hashtbl.find env (value_of_block llb,primed) in
      ZZ3.(T.symbol (Symbol.trustme Bool e))
    with Not_found -> raise @@ Block_not_found (primed, llb)



  (** Access function, that create the variable if undefined. *)

  let getVar : type a b . ?name:_ -> ?primed:_ -> (a,b) typ -> llvalue -> b term =
    fun ?(name="") ?(primed=false) typ llv ->
      try T.symbol @@ Symbol.trustme typ (Hashtbl.find env (llv,primed))
      with Not_found ->
        let name = make_name ~primed @@ (name ^ value_name llv) in
        let ty = type_of llv in
        let expr : b term = match typ, classify_type ty with
          | Real, (TypeKind.Float | TypeKind.Double) ->
              T.symbol @@ Symbol.declare Real name
          | Bool, TypeKind.Integer when integer_bitwidth ty = 1 ->
              T.symbol @@ Symbol.declare Bool name
          | Int, TypeKind.Integer ->
              T.symbol @@ Symbol.declare Int name
          | Bool, TypeKind.Label -> (* Basic blocks are boolean values *)
              T.symbol @@ Symbol.declare Bool name
          | Real,_ -> (* In case of doubt, just use a float *)
              T.symbol @@ Symbol.declare Real name
          | _ -> failwith @@ string_of_llvalue llv
        in

        Hashtbl.add env (llv,primed) (expr : b term :> Z3.Expr.expr) ;
        expr

  (** A block is a normal variable, starting by "b" and of type Bool. *)
  let getBlockVar ?(primed = false) llb =
    getVar ~primed ~name:"b" Bool @@ value_of_block llb


  (** Edges variables, starting by "e" and of type Bool. *)
  let edges = Hashtbl.create 512

  let getEdgeVar ~src ~dst =
    try Hashtbl.find edges (src,dst)
    with Not_found ->
      let name = make_name "e" in
      let e = T.symbol @@ Symbol.declare Bool name in
      Hashtbl.add edges (src,dst) e ;
      e

  (** Transform a value into an expression. Need a type annotation. *)
  let getValueExpr (type a) (type b) (typ : (a,b) typ) llv : b term =
    let open ValueKind in
    match typ, classify_value llv with
      (* Easy constants *)
      | Real, ConstantFP ->
          let value = raise (Not_implemented llv) (* float_of_const llv *) in
          T.rat (Q.of_float @@ Option.get value)
      | Int, ConstantInt ->
          let value = int64_of_const llv in
          T.bigint (Z.of_int64 @@ Option.get value)
      | Bool, ConstantInt ->
          let value = int64_of_const llv in
          T.bool (0L <> Option.get value)

      | Int, ConstantPointerNull -> T.int 0

      (* Less easy constants *)
      | _, (ConstantAggregateZero
           | ConstantArray
           | ConstantDataArray
           | ConstantDataVector
           | ConstantExpr
           | ConstantStruct
           | ConstantVector)
        -> raise @@ Not_implemented llv

      | typ, UndefValue
        -> getVar typ llv

      | typ , ( Argument | Instruction _ )
        -> getVar typ llv

      | typ, (NullValue
           | InlineAsm
           | BasicBlock
           | MDNode
           | MDString
           | BlockAddress
           | Function
           | GlobalAlias
           | GlobalVariable)
        -> getVar typ llv
      | _, _ -> raise @@ Not_implemented llv


  let getTyp llv =
    let open TypeKind in
    let ty = type_of llv in
    match classify_type ty with
      | Float | Double -> Ex Real
      | Integer when integer_bitwidth ty = 1 -> Ex Bool
      | Integer -> Ex Int
      | Label -> Ex Bool
      | _ -> Ex Real



  (** SMT variable registration. *)

  (* Function parameters need a slightly special care :
     they are the same along any path in the cfg, so we need the
     same variable for primed and not primed.
  *)
  let register_param llp =
    let (Ex typ) = getTyp llp in
    let name = make_name (value_name llp) in
    let expr = T.symbol @@ Symbol.declare typ name in
    Hashtbl.add env (llp,true) (expr :> Z3.Expr.expr) ;
    Hashtbl.add env (llp,false) (expr :> Z3.Expr.expr) ;
    ()

  (* If a block is a parameter, we need to create a primed boolean too
     This primed boolean is used to fix paths in the multi-pc version.
  *)
  let register_node llnode =
    if is_primed_block llnode
    then ignore @@ getBlockVar ~primed:true llnode.Llg.block
    else ignore @@ getBlockVar ~primed:false llnode.Llg.block


  (** Instruction translation. *)

  let binop instr typ op =
    let e_instr = getValueExpr typ instr in
    let operand0 = getValueExpr typ @@ operand instr 0 in
    let operand1 = getValueExpr typ @@ operand instr 1 in
    Some T.( e_instr = op operand0 operand1)

  let comp instr typ op =
    let e_instr = getValueExpr Bool instr in
    let operand0 = getValueExpr typ @@ operand instr 0 in
    let operand1 = getValueExpr typ @@ operand instr 1 in
    Some T.(e_instr = op operand0 operand1)

  let phi2smt ?(primed=false) llphi =
    let Ex typ = getTyp llphi in
    let inc = incoming llphi in
    let dst = instr_parent llphi in
    let free_var =
      T.symbol @@ Symbol.declare typ (make_name "phi_else") in
    let aux (llv, src) formula =
      let e_llv = getValueExpr typ llv in
      let e_edge = getEdgeVar ~src ~dst in
      T.(ite e_edge e_llv formula)
    in
    let e_phi = getVar ~primed typ llphi in
    T.(e_phi = List.fold_right aux inc free_var)

  let instr2smt llv =
    let open Opcode in
    match instr_opcode llv with
      (* Ignore these instructions *)
      | Invalid | Ret | Switch | IndirectBr
      | Unreachable | Alloca | Load | Store
      | GetElementPtr | SExt | FPTrunc | FPExt
      | PtrToInt | IntToPtr | BitCast | Call
      | Select | VAArg | ExtractElement | InsertElement
      | ShuffleVector | ExtractValue | InsertValue | Fence
      | AtomicCmpXchg | AtomicRMW | Resume | LandingPad
      | Invalid2 | Invoke -> begin
          (* Create a variable (it might be used in pagai's invariants)
             but don't return any expression. *)
          let (Ex typ) = getTyp llv in
          let _e = getValueExpr typ llv in
          None
        end

      (* Do something here *)
      | PHI -> Some (phi2smt llv)
      | Br ->
          let current_block = instr_parent llv in
          let b_curr = getBlockVar current_block in
          if num_operands llv = 1 then begin
            let succ_block = block_of_value @@ operand llv 0 in
            let edge = getEdgeVar ~src:current_block ~dst:succ_block in
            Some T.(b_curr = edge)
          end
          else begin
            let cond = getVar Bool @@ operand llv 0 in

            (* USE OF OPERAND 2 AND OPERAND 1 ARE NOT A TYPO.
               It's reverted in llvm's representation. *)
            let succ_block1 = block_of_value @@ operand llv 2 in
            let edge1 = getEdgeVar ~src:current_block ~dst:succ_block1 in

            let succ_block2 = block_of_value @@ operand llv 1 in
            let edge2 = getEdgeVar ~src:current_block ~dst:succ_block2 in

            Some T.(edge1 = (cond && b_curr) && edge2 = (not cond && b_curr))
          end
      (* Int Extension/Truncature. We don't check the size of ints atm. *)
      | ZExt | Trunc ->
          let e = getValueExpr Int llv in
          let operand = getValueExpr Int @@ operand llv 0 in
          Some T.(e = operand)
      (* Float/Int casts *)
      | FPToUI | FPToSI ->
          let e = getValueExpr Int llv in
          let operand = getValueExpr Real @@ operand llv 0 in
          Some T.(e = q2i operand)
      | UIToFP | SIToFP ->
          let e = getValueExpr Real llv in
          let operand = getValueExpr Int @@ operand llv 0 in
          Some T.(e = i2q operand)

      (* Binary operators *)
      | Add         -> binop llv Int  T.( + )
      | FAdd        -> binop llv Real T.( + )
      | Sub         -> binop llv Int  T.( - )
      | FSub        -> binop llv Real T.( - )
      | Mul         -> binop llv Int  T.( - )
      | FMul        -> binop llv Real T.( * )
      | UDiv | SDiv -> binop llv Int  T.( / )
      | FDiv        -> binop llv Real T.( / )
      | URem | SRem -> binop llv Int  T.( mod )
      | FRem        -> binop llv Real T.rem
      | And         -> binop llv Bool T.(&&)
      | Or          -> binop llv Bool T.(||)
      | Xor         -> binop llv Bool T.(lxor)
      | Shl | LShr | AShr  -> None

      | UserOp1 | UserOp2 -> None

      (* Comparisons *)
      | ICmp ->
          let open Icmp in
          Option.bind (icmp_predicate llv) (function
            | Eq  -> comp llv Int T.(=)
            | Ne  -> comp llv Int T.(<>)
            | Ugt | Sgt -> comp llv Int T.(>)
            | Uge | Sge -> comp llv Int T.(>=)
            | Ult | Slt -> comp llv Int T.(<)
            | Ule | Sle -> comp llv Int T.(<=)
          )
      | FCmp ->
          let open Fcmp in
          Option.bind (raise @@ Not_implemented llv (*fcmp_predicate llv *)) (function
            | False -> Some T.false_
            | True -> Some T.true_
            | Oeq | Ueq -> comp llv Int T.(=)
            | One | Une -> comp llv Int T.(<>)
            | Ogt | Ugt -> comp llv Int T.(>)
            | Oge | Uge -> comp llv Int T.(>=)
            | Olt | Ult -> comp llv Int T.(<)
            | Ole | Ule -> comp llv Int T.(<=)
            | Ord | Uno ->
                let b = T.symbol @@ Symbol.declare Bool @@ make_name @@ value_name llv in
                let e = getValueExpr Int llv in
                Some T.(e = b)
          )


  let llvm2smt llf cpoints llg =

    (* map node from llg to smtg *)
    let vertices = H.create 128 in

    iter_params register_param llf ;

    Llg.iter_vertex register_node llg ;

    Llg.iter_edges
      (fun src dest ->
         ignore @@ getEdgeVar ~src:src.Llg.block ~dst:dest.Llg.block
      )
      llg ;

    (* conversion function *)
    let llnode2smtnode ({Llg. id ; block ; phi ; instr } as node) =
      let primed = is_primed_block node in
      let in_formula =
        let in_ =
          List.map (fun src -> getEdgeVar ~src:src.Llg.block ~dst:block)
          @@ Llg.pred llg node
        in match in_ with
          | [] when List.mem block cpoints -> []
          | [] -> [T.not @@ getBlockVar ~primed block]
          | l -> [T.(or_ l = getBlockVar ~primed block)]
      in
      let formulas =
        in_formula @
          List.map (phi2smt ~primed) phi @
          concat_optlist @@ List.map instr2smt instr in
      {SMTg. id ; formulas }
    in
    let addnodes llnode g =
      let smtnode = llnode2smtnode llnode in
      H.add vertices llnode smtnode ;
      SMTg.add_vertex g smtnode
    in

    let addedge src dest g =
      SMTg.add_edge g (H.find vertices src) (H.find vertices dest)
    in

    SMTg.empty
    |> Llg.fold_vertex addnodes llg
    |> Llg.fold_edges addedge llg


end
