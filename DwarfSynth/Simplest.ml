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

type rbp_pos =
  | RbpUndef
  | RbpCfaOffset of memory_offset

type reg_pos = cfa_pos * rbp_pos

type reg_changes_fde = reg_pos AddrMap.t

type subroutine_cfa_data = {
  reg_changes_fde: reg_changes_fde;
  beg_pos: memory_address;
  end_pos: memory_address;
}

type block_local_state = {
  rbp_vars: BStd.Var.Set.t;
  rbp_pop_set: BStd.Tid.Set.t
}

module StrMap = Map.Make(String)
type subroutine_cfa_map = subroutine_cfa_data StrMap.t

module TIdMap = Map.Make(BStd.Tid)

exception InvalidSub
exception UnexpectedRbpSet

type synthesis_settings = {
  mutable no_rbp_undef: bool;
  mutable timers: bool
}

let __settings = {
  no_rbp_undef = false;
  timers = false
}

let pp_cfa_pos ppx = function
  | RspOffset off -> Format.fprintf ppx "RSP + (%s)" (Int64.to_string off)
  | RbpOffset off -> Format.fprintf ppx "RBP + (%s)" (Int64.to_string off)
  | CfaLostTrack -> Format.fprintf ppx "??@."

let pp_rbp_pos ppx = function
  | RbpUndef -> Format.fprintf ppx "u"
  | RbpCfaOffset off -> Format.fprintf ppx "c%+Ld" off

let pp_reg_pos ppx (cfa_pos, rbp_pos) =
  Format.fprintf ppx "(%a; %a)" pp_cfa_pos cfa_pos pp_rbp_pos rbp_pos

let pp_int64_hex ppx number =
  let mask_short = Int64.(pred (shift_left one 16)) in
  let pp_short number =
    Format.fprintf ppx "%04x" Int64.(to_int (logand number mask_short))
  in
  List.iter pp_short @@ List.map (fun x ->
      Int64.(shift_right number (16*x))) [3;2;1;0]

let pp_cfa_changes_fde ppx cfa_changes = AddrMap.iter
    (fun addr change ->
       Format.fprintf ppx "%a: %a@."
         pp_int64_hex addr
         pp_cfa_pos change)
    cfa_changes

let pp_cfa_changes ppx =
  StrMap.iter (fun fde_name entry ->
      Format.fprintf ppx "%s@\n====@\n@\n%a@." fde_name
        pp_cfa_changes_fde entry)

let pp_option_of sub_pp ppx = function
  | None -> Format.fprintf ppx "None"
  | Some x -> Format.fprintf ppx "Some %a" sub_pp x

let timer_probe probe_name =
  if __settings.timers then (
    let time = Unix.gettimeofday () in
    Format.eprintf "~~TIME~~ %s [%f]@." probe_name time
  )

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

let is_ghost_sub sub =
  (** Check whether the subroutine has content *)
  let is_ghost_block blk =
    BStd.Blk.elts blk
    |> BStd.Seq.is_empty
  in
  let blk_seq = BStd.Term.enum BStd.blk_t sub in
  BStd.Seq.for_all blk_seq ~f:is_ghost_block

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

exception Block_not_in_subroutine

let build_next_instr sub_ranges (disasm: BStd.disasm): AddrSet.t AddrMap.t =
  (** Build a map of memory_address -> AddrSet.t holding, for each address, the
      set of instructions coming right after the instruction at given address.
      There might be multiple such addresses, if the current instruction is at
      a point of branching.  *)

  let rec build_of_instr_list cur_map = function
    (** Maps an instruction to its following instruction in this block *)
    | (cur_mem, cur_insn) :: ((next_mem, next_insn) as elt2) :: tl ->
      (* Its only successor is next_insn *)
      let new_map =
        (try
           let cur_addr = to_int64_addr @@ BStd.Memory.min_addr cur_mem
           and next_addr = to_int64_addr @@ BStd.Memory.min_addr next_mem in
           AddrMap.add cur_addr (AddrSet.singleton next_addr) cur_map
         with _ -> cur_map)
        in
        build_of_instr_list new_map (elt2 :: tl)
    | (cur_mem, cur_insn) :: [] ->
      let last_addr =
        (try Some (to_int64_addr @@ BStd.Memory.min_addr cur_mem)
         with _ -> None) in
      cur_map, last_addr

      (* Ignore the last one: its successors are held in the graph *)
    | [] ->
      cur_map, None
  in

  let cfg = BStd.Disasm.cfg disasm in

  let rec block_addresses block =
    (try BStd.Block.addr block
         |> to_int64_addr
         |> AddrSet.singleton
     with _ ->
       (* Probably an intermediary node, eg. JMP --> [inermed node] --> BLK *)
       let outputs = BStd.Graphs.Cfg.Node.outputs block cfg
                     |> BStd.Seq.map ~f:BStd.Graphs.Cfg.Edge.dst in
       BStd.Seq.fold outputs
         ~init:AddrSet.empty
         ~f:(fun accu block -> AddrSet.union (block_addresses block) accu)
    )
  in

  let block_following_set block sub_first_addr sub_last_addr last_addr =
    (** Set of addresses that are successors of this block

        This set is the set of the addresses of blocks pointed by an output
        edge of this block. We filter this set so that it only contains
        addresses inside the subroutine, and take care of keeping only the
        return edge of calls.
    *)

    let blk_outputs =
       BStd.Graphs.Cfg.Node.outputs block cfg
       |> BStd.Seq.fold
         ~init:AddrSet.empty
         ~f:(fun set edge -> AddrSet.union
                (block_addresses
                   (BStd.Graphs.Cfg.Edge.dst edge))
                set)
       |> AddrSet.filter (fun addr ->
           sub_first_addr <= addr
           && addr <= sub_last_addr)
       (* ^ We must ensure the landing address belongs to the current
          subroutine for our purpose *)
    in

    let terminator = BStd.Block.terminator block in
    (* `terminator` is the only jump-ish instruction in `block` *)

    (match BStd.Insn.is BStd.Insn.call terminator with
     | true ->
       (* `terminator` is a call: we mustn't include the callee as a successor
          of this block. *)

       (* Only one output: the one that is closest to `last_addr`, forwards. *)

       (match AddrSet.find_first_opt
                (fun x -> Int64.(last_addr < x)) blk_outputs with
       | Some ret_addr ->
         let delta = Int64.sub ret_addr last_addr |> Int64.to_int in
         if delta >= 32 then
           assert false (* FIXME maybe no return *)
         else (
           (AddrSet.singleton ret_addr)
         )
       | None -> AddrSet.empty (* assume no return *)
       )

     | false ->
       (* `terminator` is not a call, all this block's outputs are correct
          successors *)
       blk_outputs
    )
  in

  let build_of_block cur_map block =
    (try
       (* First, check that this block belongs to a subroutine *)
       let block_first_address = (
         try
           to_int64_addr @@ BStd.Block.addr block
         with _ -> raise Block_not_in_subroutine) in
       let sub_first_addr, sub_last_addr = (
         try AddrMap.find_last
               (fun start_addr -> start_addr <= block_first_address) sub_ranges
         with Not_found ->
           raise Block_not_in_subroutine
       ) in

       (* Add the sequence of instuctions inside the block itself *)
       let cur_map, last_addr =
         build_of_instr_list cur_map (BStd.Block.insns block) in

       (* Then the set of possible destinations for the block terminator *)
       (match last_addr with
        | Some last_addr ->
          let following_set = block_following_set
              block sub_first_addr sub_last_addr last_addr in
          AddrMap.add last_addr following_set cur_map
        | None -> cur_map
       )
     with Block_not_in_subroutine ->
       cur_map
    )
  in

  BStd.Seq.fold (BStd.Graphs.Cfg.nodes cfg)
    ~init:AddrMap.empty
    ~f:build_of_block

type rbp_pop_state =
  | SomePop of BStd.tid
  | SomeRbpUse
  | NoPop

let find_rbp_pop_set cfg entry =
  (** Returns a BStd.Tid.Set.t of the terms actually "popping" %rbp, that is,
      the terms that should trigger a change to RbpUndef of the %rbp register.
      The current heuristic is to consider the expressions
       i) of the form `rbp = F(mem, rsp)` (alledgedly, rbp = something from the
        stack);
      ii) that are the last of this kind in the subroutine's CFG (ie. such that
        there is not another instruction matching (i) that is reachable through
        the CFG from the current instruction).
      iii) that are the last references to %rbp in a `Def` in the subroutine's
        CFG (cf (ii)).
  *)

  let def_is_rbp_pop def =
    let is_pop_expr expr =
      let free_vars = BStd.Exp.free_vars expr in
      let free_x86_regs = Regs.X86_64.map_varset free_vars in
      (match Regs.DwRegOptSet.cardinal free_x86_regs with
       | 2 ->
         let reg = free_x86_regs
                   |> Regs.DwRegOptSet.filter
                     (fun x -> match x with None -> false | Some _ -> true)
                   |> Regs.DwRegOptSet.choose in
         let has_mem_var = BStd.Var.Set.exists
             ~f:(fun x -> BStd.Var.name x = "mem")
             free_vars in
         (match reg, has_mem_var with
          | Some dw_var, true when dw_var = Regs.X86_64.rsp -> true
          | _ -> false)
       | _ -> false
      )
    in

    (match Regs.X86_64.of_var (BStd.Def.lhs def),
           is_pop_expr @@ BStd.Def.rhs def with
    | Some reg, true when reg = Regs.X86_64.rbp -> true
    | _ -> false
    )
  in

  let def_uses_rbp def =
    let def_vars = BStd.Var.Set.add
        (BStd.Def.free_vars def)
        (BStd.Def.lhs def) in
    BStd.Var.Set.exists def_vars (fun var -> match Regs.X86_64.of_var var with
        | Some x when x = Regs.X86_64.rbp -> true
        | _ -> false)
  in

  let block_find_rbp_pop block =
    let fold_elt = function
      | `Def(def) when (def_is_rbp_pop def) -> SomePop (BStd.Term.tid def)
      | `Def(def) when (def_uses_rbp def) -> SomeRbpUse
      | _ -> NoPop
    in

    let elts_seq = BStd.Blk.elts block in
    let last_pop = BStd.Seq.fold elts_seq
        ~init:NoPop
        ~f:(fun accu elt ->
            (match fold_elt elt with
             | NoPop -> accu
             | SomeRbpUse -> SomeRbpUse
             | SomePop tid -> SomePop tid))
    in
    last_pop
  in

  let rec block_dfs node visited =
    (* DFS on the CFG to find rbp pops, and rule out those that are not final
     *)
    let block = CFG.Node.label node in
    (match BStd.Blk.Set.mem visited block with
    | true ->
      (* Loop: we already visited this node *)
      BStd.Tid.Set.empty, true, visited
    | false ->
      let visited = BStd.Blk.Set.add visited block in
      let pop_set, eligible, visited =
        BStd.Seq.fold (CFG.Node.succs node cfg)
          ~f:(fun (pre_pop_set, pre_has_pop, visited) child ->
              let cur_pop_set, cur_has_pop, visited =
                block_dfs child visited in
              (BStd.Tid.Set.union pre_pop_set cur_pop_set),
              (pre_has_pop || cur_has_pop),
              visited
            )
          ~init:(BStd.Tid.Set.empty, false, visited)
      in
      let pop_set, eligible = (match eligible with
          | false -> (* No rbp pop below, we seek rbp pops in this block *)
            (match block_find_rbp_pop block with
                | NoPop -> pop_set, false
                | SomeRbpUse -> pop_set, true
                | SomePop tid -> BStd.Tid.Set.add pop_set tid, true
            )
          | true -> pop_set, eligible) in
      pop_set, eligible, visited
    )
  in

  if __settings.no_rbp_undef then
    BStd.Tid.Set.empty
  else (
    let pop_set, _, _ =
      block_dfs entry (BStd.Blk.Set.empty) in
    pop_set
  )

let interpret_var_expr c_var offset expr = BStd.Bil.(
    let closed_form = BStd.Exp.substitute
      (var c_var)
      (int (BStd.Word.of_int64 (Int64.neg offset)))
      expr
    in
    let res = BStd.Exp.eval closed_form in
    match res with
    | Imm value ->
      Some (Int64.neg @@ BStd.Word.to_int64_exn @@ BStd.Word.signed value)
    | _ -> None
  )

let is_single_free_reg expr =
  (** Detects whether `expr` contains a single free variable which is a machine
      register, and if so, extracts this register and returns a pair `(formal,
      reg`) of its formal variable in the expression and the register.
      Otherwise return None. *)
  let free_vars = BStd.Exp.free_vars expr in
  let free_x86_regs = Regs.X86_64.map_varset free_vars in
  (match Regs.DwRegOptSet.cardinal free_x86_regs with
   | 1 ->
        let free_var = Regs.DwRegOptSet.choose free_x86_regs in
        (match free_var with
         | Some dw_var ->
           let bil_var = (match BStd.Var.Set.choose free_vars with
               | None -> assert false
               | Some x -> x) in
           Some (bil_var, dw_var)
         | _ -> None
        )
   | _ -> None
  )

let process_def (local_state: block_local_state) def (cur_reg: reg_pos)
    allow_rbp
  : (reg_pos option * block_local_state) =
  let lose_track = Some CfaLostTrack in

  let cur_cfa, cur_rbp = cur_reg in
  let out_cfa =
    (match cur_cfa, Regs.X86_64.of_var (BStd.Def.lhs def), allow_rbp with
     | RspOffset(cur_offset), Some reg, _ when reg = Regs.X86_64.rsp ->
       let exp = BStd.Def.rhs def in
       (match is_single_free_reg exp with
        | Some (bil_var, dw_var) when dw_var = Regs.X86_64.rsp ->
          let interpreted = interpret_var_expr bil_var cur_offset exp in
          (match interpreted with
           | None -> lose_track
           | Some new_offset ->
             Some (RspOffset(new_offset))
          )
        | _ -> lose_track
       )
     | RspOffset(cur_offset), Some reg, true when reg = Regs.X86_64.rbp ->
       (* We have CFA=rsp+k and a line %rbp <- [expr].
          Might be a %rbp <- %rsp *)
       let exp = BStd.Def.rhs def in
       (match is_single_free_reg exp with
        | Some (bil_var, dw_var) when dw_var = Regs.X86_64.rsp ->
          (* We have %rbp := F(%rsp) *)
          (* FIXME we wish to have %rbp := %rsp. An ugly and non-robust test to
             check that would be interpret F(0), expecting that F is at worst
             affine - then a restult of 0 means that %rbp := %rsp + 0 *)
          let interpreted = interpret_var_expr bil_var (Int64.zero) exp in
          (match interpreted with
           | Some offset when offset = Int64.zero ->
             Some (RbpOffset(cur_offset))
           | _ ->
             (* Previous instruction was rsp-indexed, here we put something
                weird in %rbp, let's keep indexing with rsp and do nothing *)
             None
          )
        | _ -> None
       )
     | RbpOffset(cur_offset), Some reg, true when reg = Regs.X86_64.rbp ->
       (* Assume we are overwriting %rbp with something — we must revert to
          some rsp-based indexing *)
       (* FIXME don't assume the rsp offset will always be 8, find a smart way
          to figure this out.
          We actually use offset 16 because the `pop` will occur after the
          value is read from the stack.
       *)
       Some (RspOffset(Int64.of_int 16))
     | RbpOffset _, _, false -> assert false
     | _ -> None
    ) in

  let is_rbp_save_expr expr local_state =
    let free_vars = BStd.Exp.free_vars expr in
    let card = BStd.Var.Set.length free_vars in
    let has_mem_var = BStd.Var.Set.exists
       ~f:(fun x -> BStd.Var.name x = "mem")
       free_vars in
    let free_x86_regs = Regs.X86_64.map_varset free_vars in
    let has_rsp_var = free_x86_regs
                      |> Regs.DwRegOptSet.exists
                        (fun x -> match x with
                           | Some x when x = Regs.X86_64.rsp -> true
                           | _ -> false) in
    let has_rbp_var = free_x86_regs
                      |> Regs.DwRegOptSet.exists
                        (fun x -> match x with
                           | Some x when x = Regs.X86_64.rbp -> true
                           | _ -> false) in
    let has_intermed_rbp_var = free_vars
                             |> BStd.Var.Set.inter local_state.rbp_vars
                             |> BStd.Var.Set.is_empty
                             |> not in
    (card = 3 && has_mem_var && has_rsp_var &&
     (has_rbp_var || has_intermed_rbp_var))
  in

  let is_rbp_expr expr =
    let free_vars = BStd.Exp.free_vars expr in
    let free_x86_regs = Regs.X86_64.map_varset free_vars in
    (match Regs.DwRegOptSet.cardinal free_x86_regs with
     | 1 ->
       let reg = Regs.DwRegOptSet.choose free_x86_regs in
       (match reg with
        | Some dwreg when dwreg = Regs.X86_64.rbp -> true
        | _ -> false)
     | _ -> false)
  in

  let gather_rbp_intermed_var def cur_state =
    (* If `def` is `some intermed. var <- rbp`, add this information in the
       local state *)
    (match is_rbp_expr @@ BStd.Def.rhs def with
     | true ->
       let lhs_var = BStd.Def.lhs def in
       if (BStd.Var.is_virtual lhs_var
           && BStd.Var.typ lhs_var = BStd.reg64_t) then
         (
           (* This `def` is actually of the type we want to store. *)
           let n_rbp_vars = BStd.Var.Set.add cur_state.rbp_vars lhs_var in
           { cur_state with rbp_vars = n_rbp_vars }
         )
       else
         cur_state
     | false -> cur_state
    )
  in

  let out_rbp, new_state =
    (match cur_rbp with
     | RbpUndef ->
       let cur_state = gather_rbp_intermed_var def local_state in
       (* We assume that an expression is saving %rbp on the stack at the
          address %rip when the current def is an expression of the kind
          `MEM <- F(MEM, %rip, v)` where `v` is either `%rbp` or some
          intermediary variable holding `%rbp`.
          This approach is sound when %rbp is saved using a `push`, but
          probably wrong when saved using a `mov` on some stack-space allocated
          previously (eg. for multiple registers saved at once).
          It would be far better to actually read the position at which `v` is
          saved, but this requires parsing the actual rhs expression, which is
          not easily done: FIXME
       *)

       let new_rbp =
         if (BStd.Var.name @@ BStd.Def.lhs def = "mem"
             && is_rbp_save_expr (BStd.Def.rhs def) cur_state)
         then
           (match cur_cfa with
            | RspOffset off ->
              Some (RbpCfaOffset (Int64.mul Int64.minus_one off))
            | _ -> raise UnexpectedRbpSet
           )
         else
           None
       in

       new_rbp, cur_state
     | RbpCfaOffset offs ->
       (* We go back to RbpUndef if the current def is in the rbp_pop_set --
          see `find_rbp_pop_set` *)

       (match BStd.Tid.Set.mem (local_state.rbp_pop_set) @@ BStd.Term.tid def
            with
       | true ->
         Some RbpUndef, local_state
       | false -> None, local_state
       )
    )
  in

  (match out_cfa, out_rbp with
  | None, None -> None
  | Some cfa, None -> Some (cfa, cur_rbp)
  | None, Some rbp -> Some (cur_cfa, rbp)
  | Some cfa, Some rbp -> Some (cfa, rbp)),
  new_state

let process_jmp jmp (cur_reg: reg_pos)
  : (reg_pos option) =
  let cur_cfa, cur_rbp = cur_reg in
  let gen_change = match cur_cfa with
    | RspOffset cur_offset -> (fun off ->
        let new_offset = Int64.add cur_offset (Int64.of_int off) in
        Some (RspOffset(new_offset), cur_rbp)
      )
    | _ -> (fun _ -> None)
  in

  match (BStd.Jmp.kind jmp) with
  | BStd.Call call -> (
      (* If this call never returns (tail call), do not generate an offset of
         -8. *)
      match BStd.Call.return call with
      | Some _ -> gen_change (-8)
      | None -> None
    )
  | BStd.Ret ret -> gen_change (8)
  | _ -> None

let process_blk
    next_instr_graph rbp_pop_set allow_rbp (block_init: reg_pos) blk
  : (reg_changes_fde * reg_pos) =
  (** Extracts the registers (CFA+RBP) changes of a block. *)

  let apply_offset cur_addr_opt ((accu:reg_changes_fde), cur_reg, local_state)
    = function
      | None -> (accu, cur_reg, local_state)
      | Some reg_pos ->
        let cur_addr = (match cur_addr_opt with
            | None -> assert false
            | Some x -> to_int64_addr x) in
        (AddrSet.fold (fun n_addr cur_accu ->
             AddrMap.add n_addr reg_pos cur_accu)
            (AddrMap.find cur_addr next_instr_graph)
            accu),
        reg_pos,
        local_state
  in

  let fold_elt (accu, cur_reg, cur_local_state) elt = match elt with
    | `Def(def) ->
      let new_offset, new_state =
        process_def cur_local_state def cur_reg allow_rbp in
      apply_offset
        (opt_addr_of def) (accu, cur_reg, new_state) new_offset
    | `Jmp(jmp) ->
      apply_offset
        (opt_addr_of jmp) (accu, cur_reg, cur_local_state)
      @@ process_jmp jmp cur_reg
    | _ -> (accu, cur_reg, cur_local_state)
  in

  let init_changes = (match opt_addr_of blk with
      | None -> AddrMap.empty
      | Some x ->
        let blk_address = to_int64_addr x in
        AddrMap.singleton blk_address block_init
    ) in

  let empty_local_state = {
    rbp_vars = BStd.Var.Set.empty;
    rbp_pop_set = rbp_pop_set
  } in
  let elts_seq = BStd.Blk.elts blk in
  let out_reg, end_reg, _ = BStd.Seq.fold elts_seq
    ~init:(init_changes, block_init, empty_local_state)
    ~f:fold_elt in
  out_reg, end_reg

exception Inconsistent of BStd.tid

let get_entry_blk graph first_addr =
  let filter_out_of_range = function
    | None -> None
    | Some x when x < first_addr -> None
    | Some x -> Some x
  in
  let entry = BStd.Seq.min_elt (CFG.nodes graph) ~cmp:(fun x y ->
      let ax = filter_out_of_range @@ opt_addr_of @@ CFG.Node.label x
      and ay = filter_out_of_range @@ opt_addr_of @@ CFG.Node.label y in
      match ax, ay with
      | None, None -> compare x y
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some ax, Some ay -> compare (to_int64_addr ax) (to_int64_addr ay))
  in
  match entry with
  | None -> assert false
  | Some x -> x

let find_last_addr sub =
  (** Finds the maximal instruction address in a subroutine *)

  let map_opt fl fr merge l r = match l, r with
    | None, None -> None
    | Some x, None -> Some (fl x)
    | None, Some y -> Some (fr y)
    | Some x, Some y -> Some (merge (fl x) (fr y))
  in
  let max_opt_addr_word = map_opt
      (fun x -> x)
      (fun y -> to_int64_addr y)
      max
  in
  let max_opt_addr = map_opt
      (fun x -> x)
      (fun y -> y)
      max
  in
  let max_def cur_max def =
    max_opt_addr_word cur_max (opt_addr_of def)
  in

  let fold_res =
    BStd.Seq.fold (BStd.Term.enum BStd.blk_t sub)
      ~init:None
      ~f:(fun cur_max blk ->
          max_opt_addr
            (BStd.Seq.fold (BStd.Term.enum BStd.def_t blk)
               ~init:cur_max
               ~f:max_def)
            (BStd.Seq.fold (BStd.Term.enum BStd.jmp_t blk)
               ~init:cur_max
               ~f:max_def)
        )
  in
  match fold_res with
  | None -> Int64.zero
  | Some x -> x

let cleanup_fde (fde_changes: reg_changes_fde) : reg_changes_fde =
  (** Cleanup the result of `of_sub`.

      Merges entries at the same address, propagates track lost *)

  let fold_one addr reg_change (accu, last_change, lost_track) =
    match reg_change, last_change, lost_track with
    | _, _, true -> (accu, None, lost_track)
    | (CfaLostTrack, _), _, false ->
      (AddrMap.add addr reg_change accu, None, true)
    | reg_change, Some prev_change, false when reg_change = prev_change ->
      (accu, last_change, false)
    | reg_change, _, false ->
      (AddrMap.add addr reg_change accu, Some reg_change, false)
  in

  match AddrMap.fold fold_one fde_changes (AddrMap.empty, None, false) with
  | out, _, _ -> out


type merge_type =
    Valid_merge
  | Invalid_merge
  | Valid_with_rbp_erasing of reg_pos

let valid_merge_boilerplate regs_1 regs_2 valid_rbp_merge =
  let r1_cfa, r1_rbp = regs_1 in
  let r2_cfa, r2_rbp = regs_2 in
  (match r1_cfa = r2_cfa with
   | true -> valid_rbp_merge r1_rbp r2_rbp r1_cfa
   | false -> Invalid_merge)


let symmetric_valid_merge regs_1 regs_2 =
  let valid_rbp_merge r1 r2 cfa = (match r1, r2 with
      | x, y when x = y -> Valid_merge
      | RbpUndef, RbpCfaOffset _ -> Valid_with_rbp_erasing (cfa, r1)
      | RbpCfaOffset _, RbpUndef -> Valid_with_rbp_erasing (cfa, r2)
      | _ -> Invalid_merge)
  in
  valid_merge_boilerplate regs_1 regs_2 valid_rbp_merge

let valid_merge previous_regs cur_regs =
  let valid_rbp_merge old cur cfa = (match old, cur with
      | x, y when x = y -> Valid_merge
      | RbpUndef, RbpCfaOffset _ -> Valid_merge
      | RbpCfaOffset _, RbpUndef -> Valid_with_rbp_erasing (cfa, cur)
      | _ -> Invalid_merge)
  in
  valid_merge_boilerplate previous_regs cur_regs valid_rbp_merge

let process_sub sub next_instr_graph : subroutine_cfa_data =
  (** Extracts the `cfa_changes_fde` of a subroutine *)

  let cfg = BStd.Sub.to_cfg sub in

  let first_bap_addr = addr_of sub in
  let first_addr = to_int64_addr first_bap_addr in
  let last_addr = find_last_addr sub in

  let initial_cfa_rsp_offset = Int64.of_int 8 in

  let entry_blk = get_entry_blk cfg (first_bap_addr) in
  let rbp_pop_set = find_rbp_pop_set cfg entry_blk in


  let rec dfs_process
      allow_rbp
      (sub_changes: (reg_changes_fde * reg_pos) TIdMap.t)
      node
      (entry_offset: reg_pos) =
    (** Processes one block *)

    let cur_blk = CFG.Node.label node in
    let tid = BStd.Term.tid @@ cur_blk in

    let compute_block_and_update entry_offset sub_changes =
      let cur_blk_changes, end_reg =
        process_blk next_instr_graph rbp_pop_set
          allow_rbp entry_offset cur_blk in
      let n_sub_changes =
        TIdMap.add tid (cur_blk_changes, entry_offset) sub_changes in
      n_sub_changes, end_reg
    in


    match (TIdMap.find_opt tid sub_changes) with
    | None ->
      (* Not yet visited: compute the changes *)
      let n_sub_changes, end_reg =
        compute_block_and_update entry_offset sub_changes in

      BStd.Seq.fold (CFG.Node.succs node cfg)
        ~f:(fun accu child ->
            (match entrypoint_address (CFG.Node.label child) with
             | Some x when x < first_bap_addr -> accu
             | _ -> dfs_process allow_rbp accu child end_reg)
            )

        ~init:n_sub_changes
    | Some (_, former_entry_offset) ->
      (* Already visited: check that entry values are matching *)

      let do_fail () =
         if allow_rbp then
           Format.eprintf "Found inconsistency (0x%Lx <%a>): %a -- %a@."
             (int64_addr_of cur_blk)
             BStd.Tid.pp tid
             pp_reg_pos entry_offset pp_reg_pos former_entry_offset ;
         raise (Inconsistent tid)
      in

      (match valid_merge former_entry_offset entry_offset with
       | Valid_merge -> sub_changes

       | Invalid_merge -> do_fail ()
       | Valid_with_rbp_erasing _ ->
         (* Valid only if we manage to set back %rbp to undef in this block and
            propagate the changes.
            This tends to happen mostly in leaf blocks of a function (just
            before a return), so we only handle the case for those blocks, in
            which case there is no propagation needed.
            The easy way to do this is simply to re-synthesize the block.
         *)
         let out_degree = CFG.Node.degree ~dir:`Out node cfg in
         (match out_degree with
          | 0 ->
            let n_sub_changes, _ =
              compute_block_and_update entry_offset sub_changes in
            n_sub_changes
          | _ ->
            do_fail ()
         )
      )
  in

  let with_rbp_if_needed initial_offset =
    (* Tries first without allowing CFA=rbp+k, then allowing it if the first
       result was either inconsistent or lost track *)
    let not_losing_track synth_result =
      let lost_track = TIdMap.exists
          (fun _ (_, (cfa_pos, _)) -> match cfa_pos with
             | CfaLostTrack -> true
             | _ -> false) synth_result
      in
      (match lost_track with
       | true -> None
       | false -> Some synth_result)
    in
    let without_rbp =
      (try
         dfs_process false TIdMap.empty entry_blk initial_offset
         |> not_losing_track
       with Inconsistent _ -> None
      )
    in
    (match without_rbp with
     | Some correct_res -> correct_res
     | None ->
       dfs_process true TIdMap.empty entry_blk initial_offset)
  in


  let initial_offset = (RspOffset initial_cfa_rsp_offset, RbpUndef) in
  (* Try first without rbp, then with rbp upon failure *)
  let changes_map = with_rbp_if_needed initial_offset in

  let merged_changes = TIdMap.fold
      (fun _ (cfa_changes, _) accu -> AddrMap.union (fun addr v1 v2 ->
           match (symmetric_valid_merge v1 v2) with
           | Valid_merge -> Some v1
           | Invalid_merge ->
             Format.eprintf "Inconsistency: 0x%Lx: cannot merge %a - %a@."
               addr pp_reg_pos v1 pp_reg_pos v2 ;
             Some (CfaLostTrack, RbpUndef)
           | Valid_with_rbp_erasing valid_merge ->
             Some valid_merge
         )
           cfa_changes accu)
      changes_map
      AddrMap.empty in

  let reg_changes = cleanup_fde merged_changes in

  let output = {
    reg_changes_fde = reg_changes ;
    beg_pos = first_addr ;
    end_pos = last_addr ;
  } in

  output

let of_prog prog next_instr_graph : subroutine_cfa_map =
  (** Extracts the `cfa_changes` of a program *)
  let fold_step accu sub =
    (try
       (match is_ghost_sub sub with
        | true -> accu
        | false ->
          let subroutine_data = process_sub sub next_instr_graph in
          StrMap.add (BStd.Sub.name sub) subroutine_data accu
       )
     with
     | InvalidSub -> accu
     | Inconsistent tid ->
       Format.eprintf "Inconsistent TId %a in subroutine %s, skipping.@."
         BStd.Tid.pp tid (BStd.Sub.name sub);
       accu
    )
  in
  let subroutines = BStd.Term.enum BStd.sub_t prog in
  BStd.Seq.fold subroutines
    ~init:StrMap.empty
    ~f:fold_step

let build_sub_ranges prog: (memory_address) AddrMap.t =
  (** Builds a map mapping the first address of each subroutine to its last
      address. This map can be interpreted as a list of address ranges with
      easy fast access to a member (cf Map.S.find_first) *)

  let fold_subroutine accu sub =
    (match is_ghost_sub sub with
     | true -> accu
     | false ->
       let first_addr = int64_addr_of sub in
       let last_addr = find_last_addr sub in
       AddrMap.add first_addr (last_addr) accu
    )
  in

  let subroutines = BStd.Term.enum BStd.sub_t prog in
  BStd.Seq.fold subroutines
    ~init:AddrMap.empty
    ~f:fold_subroutine

let of_proj no_rbp_undef timers proj : subroutine_cfa_map =
  (** Extracts the `cfa_changes` of a project *)
  __settings.no_rbp_undef <- no_rbp_undef ;
  __settings.timers <- timers ;
  timer_probe "dwarfsynth generation" ;
  let prog = BStd.Project.program proj in
  let sub_ranges = build_sub_ranges prog in
  let next_instr_graph =
    build_next_instr sub_ranges (BStd.Project.disasm proj) in
  let result = of_prog prog next_instr_graph in
  timer_probe "dwarfsynth cleaning" ;
  result

let clean_lost_track_subs pre_dwarf : subroutine_cfa_map =
  (** Removes the subroutines on which we lost track from [pre_dwarf] *)
  let sub_lost_track sub_name (sub: subroutine_cfa_data) =
    not @@ AddrMap.exists (fun addr pos ->
        let cfa_pos, _ = pos in
        (match cfa_pos with
        | RspOffset _ | RbpOffset _ -> false
        | CfaLostTrack -> true))
        sub.reg_changes_fde
  in
  StrMap.filter sub_lost_track pre_dwarf
