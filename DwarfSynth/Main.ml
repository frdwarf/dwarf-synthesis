open Std

let main outfile proj =
  let pre_dwarf = Simplest.of_proj proj in
  Format.printf "%a" Frontend.pp_pre_dwarf_readelf pre_dwarf
