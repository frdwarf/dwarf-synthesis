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

type cfa_changes_fde = cfa_pos AddrMap.t

type subroutine_cfa_data = {
  cfa_changes_fde: cfa_changes_fde;
  beg_pos: memory_address;
  end_pos: memory_address;
}

module StrMap = Map.Make(String)
type subroutine_cfa_map = subroutine_cfa_data StrMap.t

module TIdMap = Map.Make(BStd.Tid)

exception InvalidSub

let pp_cfa_pos ppx = function
  | RspOffset off -> Format.fprintf ppx "RSP + (%s)" (Int64.to_string off)
  | RbpOffset off -> Format.fprintf ppx "RBP + (%s)" (Int64.to_string off)
  | CfaLostTrack -> Format.fprintf ppx "??@."

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

let process_def def (cur_cfa: cfa_pos)
  : (cfa_pos option) =
  let lose_track = Some (CfaLostTrack) in

  (match cur_cfa, Regs.X86_64.of_var (BStd.Def.lhs def) with
   | RspOffset(cur_offset), Some reg when reg = Regs.X86_64.rsp ->
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
   | RspOffset(cur_offset), Some reg when reg = Regs.X86_64.rbp ->
     (* We have CFA=rsp+k and a line %rbp <- [expr]. Might be a %rbp <- %rsp *)
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
           (* Previous instruction was rsp-indexed, here we put something weird
              in %rbp, let's keep indexing with rsp and do nothing *)
           None
        )
     | _ -> None
     )
   | RbpOffset(cur_offset), Some reg when reg = Regs.X86_64.rbp ->
       (* Assume we are overwriting %rbp with something — we must revert to
          some rsp-based indexing *)
       (* FIXME don't assume the rsp offset will always be 8, find a smart way
          to figure this out *)
       Some (RspOffset(Int64.of_int 8))
   | _ -> None)

let process_jmp jmp (cur_cfa: cfa_pos)
  : (cfa_pos option) =
  let gen_change = match cur_cfa with
    | RspOffset cur_offset -> (fun off ->
        let new_offset = Int64.add cur_offset (Int64.of_int off) in
        Some (RspOffset(new_offset))
      )
    | _ -> (fun _ -> None)
  in

  match (BStd.Jmp.kind jmp) with
  | BStd.Call call -> gen_change (-8)
  | BStd.Ret ret -> gen_change (8)
  | _ -> None

let process_blk
    next_instr_graph (block_init: cfa_pos) blk : (cfa_changes_fde * cfa_pos) =
  (** Extracts the CFA changes of a block. *)

  let apply_offset cur_addr_opt ((accu:cfa_changes_fde), cur_cfa) = function
    | None -> (accu, cur_cfa)
    | Some pos ->
      let cur_addr = (match cur_addr_opt with
          | None -> assert false
          | Some x -> to_int64_addr x) in
      (AddrSet.fold (fun n_addr cur_accu ->
           AddrMap.add n_addr pos cur_accu)
          (AddrMap.find cur_addr next_instr_graph)
          accu),
      pos
  in

  let fold_elt (accu, cur_cfa) elt = match elt with
    | `Def(def) ->
      apply_offset
        (opt_addr_of def) (accu, cur_cfa) @@ process_def def cur_cfa
    | `Jmp(jmp) ->
      apply_offset
        (opt_addr_of jmp) (accu, cur_cfa) @@ process_jmp jmp cur_cfa
    | _ -> (accu, cur_cfa)
  in

  let init_changes = (match opt_addr_of blk with
      | None -> AddrMap.empty
      | Some x ->
        let blk_address = to_int64_addr x in
        AddrMap.singleton blk_address block_init
    ) in

  let elts_seq = BStd.Blk.elts blk in
  let out, end_cfa = BStd.Seq.fold elts_seq
    ~init:(init_changes, block_init)
    ~f:fold_elt in
  out, end_cfa

exception Inconsistent of BStd.tid

let get_entry_blk graph =
  let entry = BStd.Seq.min_elt (CFG.nodes graph) ~cmp:(fun x y ->
      let ax = opt_addr_of @@ CFG.Node.label x
      and ay = opt_addr_of @@ CFG.Node.label y in
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

let cleanup_fde (fde_changes: cfa_changes_fde) : cfa_changes_fde =
  (** Cleanup the result of `of_sub`.

      Merges entries at the same address, propagates track lost *)

  let fold_one addr cfa_change (accu, last_change, lost_track) =
    match cfa_change, last_change, lost_track with
    | _, _, true -> (accu, None, lost_track)
    | CfaLostTrack, _, false ->
      (AddrMap.add addr cfa_change accu, None, true)
    | cfa_change, Some prev_change, false when cfa_change = prev_change ->
      (accu, last_change, false)
    | cfa_change, _, false ->
      (AddrMap.add addr cfa_change accu, Some cfa_change, false)
  in

  match AddrMap.fold fold_one fde_changes (AddrMap.empty, None, false) with
  | out, _, _ -> out

let process_sub sub : subroutine_cfa_data =
  (** Extracts the `cfa_changes_fde` of a subroutine *)

  let cfg = BStd.Sub.to_cfg sub in
  let next_instr_graph = build_next_instr cfg in

  let first_addr = int64_addr_of sub in
  let last_addr = find_last_addr sub in

  let initial_cfa_rsp_offset = Int64.of_int 8 in

  let rec dfs_process
      (sub_changes: (cfa_changes_fde * cfa_pos) TIdMap.t) node entry_offset =
    (** Processes one block *)

    let cur_blk = CFG.Node.label node in
    let tid = BStd.Term.tid @@ cur_blk in

    match (TIdMap.find_opt tid sub_changes) with
    | None ->
      (* Not yet visited: compute the changes *)
      let cur_blk_changes, end_cfa =
        process_blk next_instr_graph entry_offset cur_blk in
      let n_sub_changes =
        TIdMap.add tid (cur_blk_changes, entry_offset) sub_changes in
      BStd.Seq.fold (CFG.Node.succs node cfg)
        ~f:(fun accu child -> dfs_process accu child end_cfa)
        ~init:n_sub_changes
    | Some (_, former_entry_offset) ->
      (* Already visited: check that entry values are matching *)
      if entry_offset <> former_entry_offset then
        raise (Inconsistent tid)
      else
        sub_changes
  in

  let entry_blk = get_entry_blk cfg in
  let initial_offset = (RspOffset initial_cfa_rsp_offset) in
  let changes_map = dfs_process TIdMap.empty entry_blk initial_offset in

  let merged_changes = TIdMap.fold
      (fun _ (cfa_changes, _) accu -> AddrMap.union (fun _ v1 v2 ->
           if v1 = v2 then
             Some v1
           else
             assert false)
           cfa_changes accu)
    changes_map
    AddrMap.empty in

  let cfa_changes = cleanup_fde merged_changes in

  let output = {
    cfa_changes_fde = cfa_changes ;
    beg_pos = first_addr ;
    end_pos = last_addr ;
  } in

  output

let of_prog prog : subroutine_cfa_map =
  (** Extracts the `cfa_changes` of a program *)
  let fold_step accu sub =
    (try
       let subroutine_data = process_sub sub in
       StrMap.add (BStd.Sub.name sub) subroutine_data accu
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

let of_proj proj : subroutine_cfa_map =
  (** Extracts the `cfa_changes` of a project *)
  let prog = BStd.Project.program proj in
  of_prog prog
