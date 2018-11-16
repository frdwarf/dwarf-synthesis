(** Defines everything related to registers. Architecture-independant. *)

open Std

module StrMap = Map.Make(String)

(** A list of possible registers *)
type dwarf_reg =
    DwReg of int  (** A register identified by some ID *)
  | DwRegCFA      (** The Canonical Frame Address virtual register *)
  | DwRegRA       (** The Return Address virtual register *)

(** A set of options of dwarf_reg. Useful to map BAP vars onto. *)
module DwRegOptSet = Set.Make(struct
    type t = dwarf_reg option
    let compare = compare
  end)

type mem_offset = int

module RegMap = Map.Make(struct
    type t=dwarf_reg
    let compare r1 r2 = match (r1, r2) with
      | DwReg(rid1), DwReg(rid2) -> compare rid1 rid2
      | r1, r2 -> Pervasives.compare r1 r2
      (* r1 and r2 are not both DwReg, so we can safely compare
                       them using the Pervasives compare *)
  end)

type reg_loc = RegOffset of dwarf_reg * mem_offset

let is_register var =
  let physical = BStd.Var.is_physical var in
  let typ = BStd.Var.typ var in
  let register = BStd.Type.(match typ with Imm(_) -> true | _ -> false) in
  physical && register

module X86_64 = struct
  let rax = DwReg(0)
  let rdx = DwReg(1)
  let rcx = DwReg(2)
  let rbx = DwReg(3)
  let rsi = DwReg(4)
  let rdi = DwReg(5)
  let rbp = DwReg(6)
  let rsp = DwReg(7)
  let r8  = DwReg(8)
  let r9  = DwReg(9)
  let r10 = DwReg(10)
  let r11 = DwReg(11)
  let r12 = DwReg(12)
  let r13 = DwReg(13)
  let r14 = DwReg(14)
  let r15 = DwReg(15)
  let rip = DwReg(16)

  let name_map_data = [
    ("RSP", rsp);
    ("RBP", rbp);
  ] (* TODO *)

  let name_map = List.fold_left
      (fun accu (name, reg) -> StrMap.add name reg accu)
      StrMap.empty
      name_map_data

  let get_register reg = match is_register reg with
    | false -> None
    | true ->
      (try Some (StrMap.find (BStd.Var.name reg) name_map)
       with Not_found -> None)

  let of_var var =
    match is_register var with
    | false -> None
    | true -> StrMap.find_opt (BStd.Var.name var) name_map

  let map_varset varset =
    BStd.Var.Set.fold varset
      ~init:DwRegOptSet.empty
      ~f:(fun accu elt -> DwRegOptSet.add (of_var elt) accu)
end
