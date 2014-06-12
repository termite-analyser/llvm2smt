open Utils
open ZZ3
open Llvm

module Llg = Llvm_graph

exception Not_implemented of llvalue


module H = Hashtbl.Make (Llg.V)


module Init (ZZ3 : ZZ3_sigs.S) (SMTg : module type of Smt_graph.Make(ZZ3))= struct

  open ZZ3

  type ex = Ex : (_,[< zany] as 'a) typ -> ex


  let make_name =
    let counter = ref 0 in
    fun ?(primed=false) s ->
      let prime =
        if primed then "_prime" else ""
      in
      incr counter ;
      Printf.sprintf "%s%s_%i" s prime !counter

  let is_primed_block {Llg. phi ; instr } = phi <> [] && instr = []

  let env : (_, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 512

  let getVar : type a b . ?name:_ -> ?primed:_ -> (a,b) typ -> llvalue -> b term =
    fun ?(name="") ?primed typ llv ->
      try T.symbol @@ Symbol.trustme typ (Hashtbl.find env (llv,primed))
      with Not_found ->
        let name = make_name ?primed @@ (name ^ value_name llv) in
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

  let getBlockVar llb =
    getVar ~name:"b" Bool @@ value_of_block llb

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
          T.int (Z.of_int64 @@ Option.get value)

      (* Less easy constants *)
      | _, (ConstantAggregateZero
           | ConstantArray
           | ConstantDataArray
           | ConstantDataVector
           | ConstantExpr
           | ConstantPointerNull
           | ConstantStruct
           | ConstantVector)
        -> raise @@ Not_implemented llv

      | _, UndefValue
        -> raise @@ Not_implemented llv

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
      let e_llv = getVar ~name:"phi" typ llv in
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
      | Invalid2 ->
          None

      (* Do something here *)
      | PHI -> Some (phi2smt llv)
      | Br ->
          let current_block = instr_parent llv in
          let e_curr = getBlockVar current_block in
          if num_operands llv = 1 then begin
            let succ_block = block_of_value @@ operand llv 0 in
            let edge = getEdgeVar ~src:current_block ~dst:succ_block in
            Some T.(e_curr = edge)
          end
          else begin
            let cond = getVar Bool @@ operand llv 0 in

            let succ_block1 = block_of_value @@ operand llv 1 in
            let edge1 = getEdgeVar ~src:current_block ~dst:succ_block1 in

            let succ_block2 = block_of_value @@ operand llv 2 in
            let edge2 = getEdgeVar ~src:current_block ~dst:succ_block2 in

            Some T.((cond && (e_curr = edge1))  ||  ((not cond && (e_curr = edge2))))
          end
      | Invoke
      | ZExt
      | Trunc
        -> raise @@ Not_implemented llv


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


  let llvm2smt llg =

    (* map node from llg to smtg *)
    let vertices = H.create 128 in

    Llg.iter_vertex
      (fun llnode ->
         ignore @@ getBlockVar llnode.Llg.block
      )
      llg ;

    Llg.iter_edges
      (fun src dest ->
         ignore @@ getEdgeVar ~src:src.Llg.block ~dst:dest.Llg.block
      )
      llg ;

    (* conversion function *)
    let llnode2smtnode ({Llg. id ; block ; phi ; instr } as node) =
      let in_formula =
        let in_ =
          List.map (fun src -> getEdgeVar ~src:src.Llg.block ~dst:block)
          @@ Llg.pred llg node
        in match in_ with [] -> T.true_ | l -> T.or_ l
      in
      let primed = is_primed_block node in
      let formulas =
        [in_formula] @
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
