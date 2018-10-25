open Std
module CFG = BStd.Graphs.Ir

type memory_offset = int64
type memory_address = int64

module AddrMap = Map.Make(Int64)
module AddrSet = Set.Make(Int64)

type cfa_pos =
    RspOffset of memory_offset
  | RbpOffset of memory_offset
  | CfaLostTrack

type cfa_change = CfaChange of memory_address * cfa_pos

type cfa_changes_fde = cfa_change list

module StrMap = Map.Make(String)
type cfa_changes = cfa_changes_fde StrMap.t

module TIdMap = Map.Make(BStd.Tid)

exception InvalidSub

let pp_cfa_pos ppx = function
  | RspOffset off -> Format.fprintf ppx "RSP + (%s)@." (Int64.to_string off)
  | RbpOffset off -> Format.fprintf ppx "RBP + (%s)@." (Int64.to_string off)
  | CfaLostTrack -> Format.fprintf ppx "??@."

let pp_int64_hex ppx number =
  let mask_short = Int64.(pred (shift_left one 16)) in
  let pp_short number =
    Format.fprintf ppx "%04x" Int64.(to_int (logand number mask_short))
  in
  List.iter pp_short @@ List.map (fun x ->
      Int64.(shift_right number (16*x))) [3;2;1;0]

let pp_cfa_change ppx = function CfaChange(addr, cfa_pos) ->
  Format.fprintf ppx "%a: %a" pp_int64_hex addr pp_cfa_pos cfa_pos

let pp_cfa_changes_fde ppx = List.iter (pp_cfa_change ppx)

let pp_cfa_changes ppx =
  StrMap.iter (fun fde_name entry ->
      Format.fprintf ppx "%s@\n====@\n@\n%a@." fde_name
        pp_cfa_changes_fde entry)

let pp_option_of sub_pp ppx = function
  | None -> Format.fprintf ppx "None"
  | Some x -> Format.fprintf ppx "Some %a" sub_pp x

let opt_addr_of term =
  (** Get the address of a term as an option, if it has one*)
  BStd.Term.get_attr term BStd.address

let addr_of term =
  (** Get the address of a term *)
  match opt_addr_of term with
  | None -> assert false
  | Some addr -> addr

let opt_addr_of_blk_elt = function
  | `Def def -> opt_addr_of def
  | `Jmp jmp -> opt_addr_of jmp
  | `Phi phi -> opt_addr_of phi

let entrypoint_address blk =
  (** Find the first instruction address in the current block.
      Return None if no instruction has address.  *)
  let fold_one accu cur_elt = match accu, opt_addr_of_blk_elt cur_elt with
    | None, None -> None
    | None, Some x -> Some x
    | _, _ -> accu
  in

  BStd.Seq.fold (BStd.Blk.elts blk)
    ~init:None
    ~f:fold_one

let to_int64_addr addr =
  BStd.Word.to_int64_exn addr

let int64_addr_of x = to_int64_addr @@ addr_of x

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let build_next_instr graph =
  (** Build a map of memory_address -> AddrSet.t holding, for each address, the
      set of instructions coming right after the instruction at given address.
      There might be multiple such addresses, if the current instruction is at
      a point of branching.  *)

  let addresses_in_block blk =
    (** Set of addresses present in the block *)
    BStd.Seq.fold (BStd.Blk.elts blk)
      ~init:AddrSet.empty
      ~f:(fun accu elt ->
          let addr = opt_addr_of_blk_elt elt in
          match addr with
          | None -> accu
          | Some x ->
            (try
               AddrSet.add (BStd.Word.to_int64_exn x) accu
             with _ -> accu)
        )
  in

  let node_successors_addr (nd: CFG.node) : AddrSet.t =
    let rec do_find_succ accu nd =
      let fold_one accu c_node =
        match entrypoint_address (CFG.Node.label c_node) with
        | Some addr ->
          (try
             AddrSet.add (BStd.Word.to_int64_exn addr) accu
           with _ -> accu)
        | None -> do_find_succ accu c_node
      in

      let succ = CFG.Node.succs nd graph in
      BStd.Seq.fold succ
        ~init:accu
        ~f:fold_one
    in
    do_find_succ AddrSet.empty nd
  in

  let build_of_block accu_map node =
    let blk = CFG.Node.label node in
    let node_successors = node_successors_addr node in
    let instr_addresses = AddrSet.elements @@ addresses_in_block blk in

    let rec accumulate_mappings mappings addr_list = function
      | None -> mappings
      | Some (instr, instr_seq) as cur_instr ->
        let instr_addr = opt_addr_of_blk_elt instr in
        match (map_option to_int64_addr instr_addr), addr_list with
        | None, _ ->
          accumulate_mappings mappings addr_list @@ BStd.Seq.next instr_seq
        | Some cur_addr, next_addr::t when cur_addr >= next_addr ->
          accumulate_mappings mappings t cur_instr
        | Some cur_addr, next_addr::_ ->
          let n_mappings = AddrMap.add
              cur_addr (AddrSet.singleton next_addr) mappings in
          accumulate_mappings n_mappings addr_list @@ BStd.Seq.next instr_seq
        | Some cur_addr, [] ->
          let n_mappings = AddrMap.add
              cur_addr node_successors mappings in
          accumulate_mappings n_mappings addr_list @@ BStd.Seq.next instr_seq
    in
    accumulate_mappings
      accu_map
      instr_addresses
      (BStd.Seq.next @@ BStd.Blk.elts blk)
  in

  BStd.Seq.fold (CFG.nodes graph)
    ~init:AddrMap.empty
    ~f:build_of_block

let interpret_var_expr c_var offset expr = BStd.Bil.(
    let closed_form = BStd.Exp.substitute
      (var c_var)
      (int (BStd.Word.of_int64 offset))
      expr
    in
    let res = BStd.Exp.eval closed_form in
    match res with
    | Imm value ->
      Some (BStd.Word.to_int64_exn @@ BStd.Word.signed value)
    | _ -> None
  )

let process_def def (cur_offset: memory_offset)
  : ((cfa_pos * memory_offset) option) =
  let lose_track = Some (CfaLostTrack, Int64.zero) in
  (match Regs.X86_64.of_var (BStd.Def.lhs def) with
   | Some reg when reg = Regs.X86_64.rsp ->
     let exp = BStd.Def.rhs def in
     let free_vars = BStd.Exp.free_vars exp in
     let free_x86_regs = Regs.X86_64.map_varset free_vars in
     (match Regs.DwRegOptSet.cardinal free_x86_regs with
      | 1 ->
        let free_var = Regs.DwRegOptSet.choose free_x86_regs in
        (match free_var with
         | Some dw_var when dw_var = Regs.X86_64.rsp ->
           let bil_var = (match BStd.Var.Set.choose free_vars with
               | None -> assert false
               | Some x -> x) in
           let interpreted = interpret_var_expr bil_var cur_offset exp in
           (match interpreted with
            | None -> lose_track
            | Some interp_val ->
              let gap = Int64.sub interp_val cur_offset in
              let new_offset = Int64.sub cur_offset gap in
              Some (RspOffset(new_offset), new_offset)
           )
         | _ -> lose_track
        )
      | _ -> lose_track
     )
   | _ -> None)

let process_jmp jmp (cur_offset: memory_offset)
  : ((cfa_pos * memory_offset) option) =
  let gen_change off =
    let new_offset = Int64.add cur_offset (Int64.of_int off) in
    Some (RspOffset(new_offset), new_offset)
  in

  match (BStd.Jmp.kind jmp) with
  | BStd.Call call -> gen_change (-8)
  | BStd.Ret ret -> gen_change (8)
  | _ -> None

let sym_of_blk next_instr_graph blk : cfa_changes_fde =
  (** Extracts the symbolic CFA changes of a block. These changes assume that
      at the beginning of the block, CFA = RspOffset(0) and will be offset
      after *)

  let apply_offset cur_addr_opt ((accu:cfa_change list), cur_offset) = function
    | None -> (accu, cur_offset)
    | Some (pos, n_offset) ->
      let cur_addr = (match cur_addr_opt with
          | None -> assert false
          | Some x -> to_int64_addr x) in
      (AddrSet.fold (fun n_addr cur_accu ->
        let change = CfaChange(n_addr, pos) in
        (change :: cur_accu))
        (AddrMap.find cur_addr next_instr_graph)
        accu),
      n_offset
  in

  let fold_elt (accu, cur_offset) elt = match elt with
    | `Def(def) ->
      apply_offset
        (opt_addr_of def) (accu, cur_offset) @@ process_def def cur_offset
    | `Jmp(jmp) ->
      apply_offset
        (opt_addr_of jmp) (accu, cur_offset) @@ process_jmp jmp cur_offset
    | _ -> (accu, cur_offset)
  in

  let elts_seq = BStd.Blk.elts blk in
  let out, end_offset = BStd.Seq.fold elts_seq
    ~init:([], Int64.zero)
    ~f:fold_elt in
  out

let end_offset (changelist: cfa_changes_fde): memory_offset option =
  match changelist with
  | CfaChange(_, RspOffset(x)) :: _ -> Some x
  | _ -> None

exception Inconsistent of BStd.tid

let rec dfs_propagate changemap propagated parent_val node graph =
  let c_tid = BStd.Term.tid @@ CFG.Node.label node in
  match TIdMap.find_opt c_tid propagated with
  | Some x ->
    if x = parent_val then
      (* Already propagated and consistent, all fine *)
      propagated
    else
      (* Already propagated with a different value: inconsistency *)
      raise (Inconsistent c_tid)
  | None ->
    let n_propagated = TIdMap.add c_tid parent_val propagated in
    let outwards = CFG.Node.outputs node graph in
    let self_entry = TIdMap.find c_tid changemap in
    let offset = (match end_offset self_entry with
          Some x -> x
        | None -> Int64.zero) in
    let cur_val = Int64.add parent_val offset in
    BStd.Seq.fold outwards
      ~init:n_propagated
      ~f:(fun accu edge ->
          dfs_propagate changemap accu cur_val (CFG.Edge.dst edge) graph)

let get_entry_blk graph =
  let entry =
    BStd.Seq.find (CFG.nodes graph)
      (fun node -> BStd.Seq.is_empty @@ CFG.Node.inputs node graph)
  in match entry with
  | None -> assert false
  | Some x -> x

let same_keys map1 map2 =
  let exists_in_other other key _ =
    TIdMap.mem key other in

  TIdMap.for_all (exists_in_other map2) map1
  && TIdMap.for_all (exists_in_other map1) map2

let of_sub sub : cfa_changes_fde =
  (** Extracts the `cfa_changes_fde` of a subroutine *)

  Format.eprintf "Sub %s...@." @@ BStd.Sub.name sub ;

  let cfg = BStd.Sub.to_cfg sub in
  let next_instr_graph = build_next_instr cfg in

  let initial_cfa_rsp_offset = Int64.of_int 8 in
  let store_sym accu blk =
    let blk = CFG.Node.label blk in
    let res = sym_of_blk next_instr_graph blk in
    TIdMap.add (BStd.Term.tid blk) res accu
  in

  let node_addr nd = opt_addr_of @@ CFG.Node.label nd in

  let merge_corrected blk_tid changes offset = match (changes, offset) with
    | Some changes, Some offset ->
      Some (
        List.map (fun (CfaChange(addr, pos)) -> match pos with
              RspOffset(off) -> CfaChange(addr,
                                          RspOffset(Int64.add off offset))
            | RbpOffset(off) -> CfaChange(addr, RbpOffset(off))
            | CfaLostTrack -> CfaChange(addr, CfaLostTrack)
          )
          changes
      )
    | _ -> None
  in

  let tid_match = BStd.Seq.fold (CFG.nodes cfg)
      ~init:TIdMap.empty
      ~f:(fun accu node ->
          let tid = BStd.Term.tid @@ CFG.Node.label node in
          TIdMap.add tid node accu)
  in

  let blk_sym = BStd.Seq.fold
      ~init:TIdMap.empty
      ~f:store_sym
      @@ CFG.nodes cfg
  in

  let entry_blk = get_entry_blk cfg in
  let offset_map = dfs_propagate
      blk_sym (TIdMap.empty) initial_cfa_rsp_offset entry_blk cfg in

  let corrected = TIdMap.merge merge_corrected blk_sym offset_map in

  let is_connex = same_keys tid_match corrected in
  if not is_connex then
    raise InvalidSub ;

  let tid_list = TIdMap.bindings tid_match in
  let sorted_blk = List.sort (fun (tid1, bl1) (tid2, bl2) ->
      let res = match (node_addr bl1, node_addr bl2) with
        | Some addr1, Some addr2 -> compare addr1 addr2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> compare tid1 tid2
      in
      -res)
      tid_list
  in

  let out = List.fold_left
      (fun accu blk ->
         let changes = TIdMap.find blk corrected in
         List.fold_left (fun accu chg -> chg::accu)
           accu
           changes
      )
      []
      (List.map (fun (x, y) -> x) sorted_blk) in

  let init = [
    CfaChange(int64_addr_of sub, RspOffset(initial_cfa_rsp_offset))
  ] in
  init @ out

let cleanup_fde (fde_changes: cfa_changes_fde) : cfa_changes_fde =
  (** Cleanup the result of `of_sub`.

      Merges entries at the same address, propagates track lost *)

  let fold_one (accu, (last_commit:cfa_pos option), in_flight, lost_track) = function
    | CfaChange(addr, cfa_change) as cur_change -> (
        match lost_track, in_flight, cfa_change with
        | true, _, _ ->
          (* Already lost track: give up *)
          (accu, last_commit, None, lost_track)
        | false, _, CfaLostTrack ->
          (* Just lost track: give up the operation on flight as well *)
          (cur_change :: accu, None, None, true)
        | _, Some CfaChange(flight_addr, flight_chg), _
          when flight_addr = addr ->
          (* On flight address matches current address: continue flying *)
            accu, last_commit, Some cur_change, false
        | _, Some CfaChange(_, in_flight_inner_pos), _
          when last_commit = Some in_flight_inner_pos ->
          (* Doesn't match anymore, but there was some operation in flight,
             which has the same result as what was last committed. Discard. *)
          (accu, last_commit, Some cur_change, false)
        | _, Some (CfaChange(_, in_flight_inner_pos) as in_flight_inner), _ ->
          (* Doesn't match anymore, but there was some operation in flight:
             commit it, put the new one in flight *)
          (in_flight_inner :: accu, Some in_flight_inner_pos,
           Some cur_change, false)
        | _, None, _ ->
          (* No operation in flight: put the new one in flight *)
          (accu, last_commit, Some cur_change, false)
    )
  in

  let extract_end_value (accu, _, in_flight, lost_track) =
    List.rev @@ match lost_track, in_flight with
    | true, _ -> accu
    | false, None -> accu
    | false, Some x -> x :: accu
  in

  extract_end_value
    @@ List.fold_left fold_one ([], None, None, false) fde_changes


let of_prog prog : cfa_changes =
  (** Extracts the `cfa_changes` of a program *)
  let fold_step accu sub =
    (try
       let res = cleanup_fde @@ of_sub sub in
       StrMap.add (BStd.Sub.name sub) res accu
     with InvalidSub -> accu)
  in
  let subroutines = BStd.Term.enum BStd.sub_t prog in
  BStd.Seq.fold subroutines
    ~init:StrMap.empty
    ~f:fold_step

let of_proj proj : cfa_changes =
  (** Extracts the `cfa_changes` of a project *)
  let prog = BStd.Project.program proj in
  of_prog prog
