open Std

let main ?no_rbp_undef:(no_rbp_undef=false) outfile proj =
  let pre_dwarf = proj
                  |> Simplest.of_proj no_rbp_undef
                  |> Simplest.clean_lost_track_subs in
  Format.printf "%a" Frontend.pp_pre_dwarf_readelf pre_dwarf;
  let pre_c_dwarf = PreCBinding.convert_pre_c pre_dwarf in
  let fd = open_out_bin outfile in
  Marshal.to_channel fd pre_c_dwarf []
