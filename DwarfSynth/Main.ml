open Std

let main outfile proj =
  let pre_dwarf = Simplest.of_proj proj in
  Format.printf "%a" Frontend.pp_pre_dwarf_readelf pre_dwarf;
  let pre_c_dwarf = PreCBinding.convert_pre_c pre_dwarf in 
  let fd = open_out_bin "tmp.marshal" in
  Marshal.to_channel fd pre_c_dwarf []
    
                    
