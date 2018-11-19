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

let pp_cfa_change ppx addr pos = Simplest.(
    let num_len num =
      let str_rep = Format.sprintf "%+d" num in
      String.length str_rep
    in
    let print_row cfa_reg int64_offset =
      let offset = Int64.to_int int64_offset in
      let post_offset_spaces = String.make (6 - num_len offset) ' ' in
      Format.fprintf ppx "%a %s%+d%sc-8@."
        pp_int64_hex addr cfa_reg offset post_offset_spaces
    in

    match pos with
    | RspOffset offset ->
      print_row "rsp" offset
    | RbpOffset offset ->
      print_row "rbp" offset
    | CfaLostTrack ->
      Format.fprintf ppx "%a u        u@." pp_int64_hex addr
  )

let pp_pre_dwarf_readelf ppx pre_dwarf =
  Simplest.StrMap.iter (fun fde_name entry ->
      Format.fprintf ppx "FDE %s@." fde_name ;
      if not (Simplest.AddrMap.is_empty entry) then (
         Format.fprintf ppx "   LOC           CFA      ra@." ;
         Simplest.AddrMap.iter (pp_cfa_change ppx) entry ;
         Format.fprintf ppx "@.")
    )
    pre_dwarf
