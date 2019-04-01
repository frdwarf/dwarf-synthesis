(** Frontend
    Clean printers, etc.
 *)

let pp_int64_hex ppx number =
  let mask_short = Int64.(pred (shift_left one 16)) in
  let pp_short number =
    Format.fprintf ppx "%04x" Int64.(to_int (logand number mask_short))
  in
  List.iter pp_short @@ List.map (fun x ->
      Int64.(shift_right number (16*x))) [3;2;1;0]

exception LostTrackCfaDisp

let pp_cfa_change ppx addr reg_pos = Simplest.(
    let num_len num =
      let str_rep = Format.sprintf "%+d" num in
      String.length str_rep
    in
    let print_row cfa_reg cfa_int64_offset rbp_offset =
      let cfa_offset = Int64.to_int cfa_int64_offset in
      let post_cfa_offset_spaces = String.make (6 - num_len cfa_offset) ' ' in
      let rbp_str = (match rbp_offset with
          | None -> "u     "
          | Some off64 ->
            let off = Int64.to_int off64 in
            Format.sprintf "c%+d%s" off (String.make (5 - num_len off) ' ')
        ) in
      Format.fprintf ppx "%a %s%+d%s%sc-8@."
        pp_int64_hex addr cfa_reg cfa_offset post_cfa_offset_spaces rbp_str
    in

    let cfa_pos, rbp_pos = reg_pos in

    (try
       let cfa_reg, cfa_offset = (match cfa_pos with
           | RspOffset offset ->
             "rsp", offset
           | RbpOffset offset ->
             "rbp", offset
           | CfaLostTrack ->
             raise LostTrackCfaDisp
         ) in
       let rbp_offset = (match rbp_pos with
           | RbpUndef -> None
           | RbpCfaOffset off -> Some off
         ) in
       print_row cfa_reg cfa_offset rbp_offset

     with LostTrackCfaDisp ->
       Format.fprintf ppx "%a u        u     u@." pp_int64_hex addr
    )
  )

let pp_pre_dwarf_readelf ppx (pre_dwarf: Simplest.subroutine_cfa_map) =
  Simplest.(
    Simplest.StrMap.iter (fun fde_name entry ->
        Format.fprintf ppx "FDE %s pc=%a..%a@."
          fde_name pp_int64_hex entry.beg_pos pp_int64_hex entry.end_pos;
        let reg_entry = entry.reg_changes_fde in
        if not (Simplest.AddrMap.is_empty reg_entry) then (
          Format.fprintf ppx "   LOC           CFA      rbp   ra@." ;
          Simplest.AddrMap.iter (pp_cfa_change ppx) reg_entry ;
          Format.fprintf ppx "@.")
      )
      pre_dwarf
  )
