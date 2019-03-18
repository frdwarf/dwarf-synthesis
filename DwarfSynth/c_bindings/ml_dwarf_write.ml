(* copy here as quick hack / restructure the file directory *)

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

let dump_pre_c_pre_dwarf_entry e =
  Printf.printf "  %8Lx  %d+%Ld \n" e.location e.cfa_offset_reg e.cfa_offset

let dump_pre_c_pre_dwarf_fde f =
  Printf.printf "%s  %Lx  %Lx\n" f.name f.initial_location f.end_location;
  for i = 0 to Array.length f.entries - 1 do
    dump_pre_c_pre_dwarf_entry f.entries.(i)
  done


let dump_pre_c_pre_dwarf p =
  for i = 0 to Array.length p.fdes - 1 do
    dump_pre_c_pre_dwarf_fde p.fdes.(i)
  done


external write_dwarf : string -> string -> pre_c_pre_dwarf -> int
  = "caml_write_dwarf"

(** ========== USAGE ========== **)
let use_string =
  Sys.argv.(0) ^ " <marshalled_data> <executable> [<eh_file>]\n" ^
  "<eh_file> defaults to `<executable>.eh_frame`\n" ;;

let _ =
  if Array.length Sys.argv <= 2 then (
    Format.eprintf "Error: missing argument(s). Usage:\n%s" use_string ;
    exit 1
  ) ;
  let eh_path = (if Array.length Sys.argv <= 3
                 then (Sys.argv.(2) ^ ".eh_frame")
                 else Sys.argv.(3)) in
  let fd = open_in_bin Sys.argv.(1) in
  let pre_c_dwarf = ((Marshal.from_channel fd): pre_c_pre_dwarf) in
  dump_pre_c_pre_dwarf pre_c_dwarf;
  write_dwarf Sys.argv.(2) eh_path pre_c_dwarf

