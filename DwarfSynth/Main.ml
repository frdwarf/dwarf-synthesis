open Std

let main outfile proj =
  let pre_dwarf = Simplest.of_proj proj in
  Format.printf "%a" Simplest.pp_cfa_changes pre_dwarf
