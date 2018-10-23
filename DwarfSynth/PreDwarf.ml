(** A program counter value *)
type pc = int

(** A notice that the register save location has changed to this new value,
    starting from the given pc *)
type reg_change = pc * Regs.reg_loc

(** A structure holding, for each register, a list of changes. Used to track
    the evolution of the storage location of every register. *)
type reg_data = reg_change list Regs.RegMap.t

(** Represents a FDE (that is, morally, a function). *)
type fde = {
  start_pc: pc;
  end_pc: pc;
  name: string;
}

(** A map of FDEs *)
module FdeMap = Map.Make(struct
    type t = fde
    let compare = compare
  end)

type pre_dwarf_data = reg_data FdeMap.t
