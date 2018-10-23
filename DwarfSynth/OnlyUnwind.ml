(** Basic Pre-DWARF generation, only for unwinding data. Tracks only CFA,
    and RA. *)

open Std
module CFG = BStd.Graphs.Ir

module BlkMap = Map.Make(BStd.Blk)

(** Add a new row (ie. register change) to a PreDwarf.reg_data table *)
let add_fde_row reg row (data: PreDwarf.reg_data) =
  let prev_data = match Regs.RegMap.find_opt reg data with
    | None -> []
    | Some l -> l
  in
  let new_data = row :: prev_data in
  Regs.RegMap.add reg new_data data

let fold_sub init fct_phi fct_def fct_jmp term =
  (** Fold over the addresses of encountered subterms in `term`, a `sub` *)
  let fold_term cls init fct cterm =
    BStd.Seq.fold ~init:init ~f:fct @@ BStd.Term.enum cls cterm
  in

  fold_term BStd.blk_t init (fun accu -> fun blk ->
      let accu_phi = fold_term BStd.phi_t accu fct_phi blk in
      let accu_def = fold_term BStd.def_t accu_phi fct_def blk in
      let accu_jmp = fold_term BStd.jmp_t accu_def fct_jmp blk in
      accu_jmp) term

exception No_address

let get_term_addr term = match (BStd.Term.get_attr term BStd.address) with
  (** Get the address of a given term *)
  | None -> raise No_address
  | Some addr -> addr

let extract_addr extractor term =
  let examine_addr paddr cterm =
    let naddr = get_term_addr cterm in extractor paddr naddr
  in
  let start_addr = get_term_addr term in
  fold_sub start_addr examine_addr examine_addr examine_addr term

let low_addr term =
  (** Find the low addr of some `BStd.term` *)
  extract_addr min term

let high_addr term =
  (** Find the high addr of some `BStd.term` (inclusive) *)
  extract_addr max term

let addr_to_int addr =
  (** Transforms an address (BStd.addr) into an int *)
  try CKStd.Or_error.ok_exn @@ BStd.Word.to_int addr
  with _ -> raise No_address

let fde_of_subroutine (sub: BStd.sub BStd.term): PreDwarf.fde =
  PreDwarf.({
    start_pc = addr_to_int @@ low_addr sub;
    end_pc = addr_to_int @@ high_addr sub;
    name = BStd.Term.name sub
  })

let symbolic_predwarf_of_blk (blk: BStd.blk BStd.term) =
  (** Analyze a block of code, and return its predwarf based on RSP, assuming
      that at the beginning of the block, CFA = %rsp.

      This will be then adjusted to be offseted by the actual CFA value upon
      entrance in this block.
  
      This function returns its offset by the end of its block.
  *)

  let fold_def accu elt =
    let var = BStd.Def.lhs elt in

    match Regs.X86_64.get_register var with
    | None -> accu
    | Some reg ->
      accu (* TODO *)
  in

  let fold_elt accu elt = match elt with
    | `Def(def) -> fold_def accu def
    | _ -> accu
  in

  BStd.Seq.fold (BStd.Blk.elts blk)
    ~init:Regs.RegMap.empty
    ~f:fold_elt

let predwarf_of_sub
    (fde:PreDwarf.fde)
    (sub: BStd.sub BStd.term)
  : PreDwarf.reg_data =
  (** Compute the pre-dwarf data for a single subroutine/FDE *)

  (* A `call` always result in %rsp pointing to the RA *)
  let init_cfa =
    [(PreDwarf.(fde.start_pc), Regs.(RegOffset(X86_64.rsp, 8)))] in
  let init_ra =
    [(PreDwarf.(fde.start_pc), Regs.(RegOffset(DwRegCFA, -8)))] in
  let init_reg_data = Regs.(RegMap.(
      add DwRegRA init_ra
      @@ add DwRegCFA init_cfa
      @@ empty
    ))
  in

  let sub_cfg = BStd.Sub.to_cfg sub in

  assert false

let compute_pre_dwarf proj: PreDwarf.pre_dwarf_data =
  let prog = BStd.Project.program proj in
  let subroutines = BStd.Term.enum BStd.sub_t prog in
  let subdwarf = BStd.Seq.fold subroutines
      ~init:PreDwarf.FdeMap.empty
      ~f:(fun accu sub ->
          let fde = fde_of_subroutine sub in
          let predwarf = predwarf_of_sub fde sub in
          PreDwarf.FdeMap.add fde predwarf accu) in
  subdwarf
