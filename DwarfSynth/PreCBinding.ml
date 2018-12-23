open Simplest
open Std

type pre_c_pre_dwarf_entry = {
  location: int64;
  cfa_offset: int64;
  cfa_offset_reg: int
}

type pre_c_pre_dwarf_fde = {
  num: int;
  initial_location: int64;
  end_location: int64;
  name: string;
  entries: pre_c_pre_dwarf_entry array
}

type pre_c_pre_dwarf = {
  num_fde: int;
  fdes: pre_c_pre_dwarf_fde array
}

(* ========================================================================= *)

(** Empty default value for `pre_c_pre_dwarf_entry` *)
let empty_entry = {
  location = Int64.zero;
  cfa_offset = Int64.zero;
  cfa_offset_reg = 0
}

(** Empty default value for `pre_c_pre_dwarf_fde` *)
let empty_fde = {
  num = 0;
  initial_location = Int64.zero;
  end_location = Int64.zero;
  name = "";
  entries = Array.make 0 empty_entry
}

module MapTool (MapT: Map.S) = struct
  (** Folds a Map.S.t into an array *)
  let fold_map_to_array folder map any_elt =
    let out = Array.make (MapT.cardinal map) any_elt in
    MapT.fold (fun key elt pos ->
        Array.set out pos @@ folder key elt pos;
        pos + 1)
      map 0
    |> ignore;
    out
end

let convert_pre_c_entry loc entry : pre_c_pre_dwarf_entry =
  let offset, offset_reg = (match entry with
      | RspOffset off -> off, 7
      | RbpOffset off -> off, 6
      | CfaLostTrack -> assert false (* Should be filtered out beforehand *)
    ) in
  {
    location = loc;
    cfa_offset = offset;
    cfa_offset_reg = offset_reg;
  }

module AddrMapTool = MapTool(AddrMap)
let convert_pre_c_entries entries : pre_c_pre_dwarf_entry array =
  AddrMapTool.fold_map_to_array
      (fun loc entry _ -> convert_pre_c_entry loc entry)
      entries empty_entry

let convert_pre_c_fde name entry id : pre_c_pre_dwarf_fde =
  {
    num = AddrMap.cardinal entry.cfa_changes_fde;
    initial_location = entry.beg_pos;
    end_location = entry.end_pos;
    name = name;
    entries = convert_pre_c_entries entry.cfa_changes_fde
  }

module StrMapTool = MapTool(StrMap)
let convert_pre_c (cfa_map: subroutine_cfa_map) : pre_c_pre_dwarf =
  (** Converts a `subroutine_cfa_map` to a `pre_c_pre_dwarf` type, in
      preparation for C coversion. *)
  let num_fde = StrMap.cardinal cfa_map in
  let fdes = StrMapTool.fold_map_to_array
      convert_pre_c_fde cfa_map empty_fde in
  {
    num_fde = num_fde ;
    fdes = fdes
  }
